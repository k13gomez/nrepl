(ns nrepl.transport
  "Abstractions and implementations for nREPL message transports.

  A transport handles the wire-level communication between client and
  server -- encoding, sending, and receiving messages. nREPL ships with
  bencode, EDN, and TTY transport implementations.

  Also provides helper functions for middleware authors: `respond-to`
  for sending responses and `safe-handle` for op dispatch with error
  handling."
  {:author "Chas Emerick"
   :added  "0.4"}
  (:refer-clojure :exclude [send #?@(:clje [locking printf])])
  (:require
   [clojure.edn :as edn]
   #?(:clj [clojure.java.io :as io])
   [clojure.string :as str]
   [nrepl.bencode :as bencode]
   [nrepl.misc :refer [response-for uuid ignore-exceptions]]
   [nrepl.socket :as socket]
   [nrepl.util.session :as session-util]
   [nrepl.util.threading :as threading]
   nrepl.version
   #?@(:clje [[nrepl.beam.bytes :as bytes]
              [nrepl.beam.compat :refer [locking printf]]
              [nrepl.beam.queue :as queue]
              [nrepl.beam.socket :as beam-socket]]))
  #?(:clj
     (:import
      (clojure.lang LineNumberingPushbackReader)
      (java.io ByteArrayOutputStream
               Closeable
               EOFException
               Flushable
               PushbackInputStream)
      [java.net SocketException]
      [java.nio.channels ClosedChannelException]
      [java.util.concurrent BlockingQueue LinkedBlockingQueue Semaphore SynchronousQueue TimeUnit])))

(defprotocol Transport
  "Defines the interface for a wire protocol implementation for use
   with nREPL."
  (recv [this] [this timeout]
    "Reads and returns the next message received.  Will block.
     Should return nil if a message is not available after `timeout`
     ms or if the underlying channel has been closed.")
  (send [this msg] "Sends msg. Implementations should return the transport."))

(deftype FnTransport [recv-fn send-fn close]
  Transport
  (send [this msg] (send-fn msg) this)
  (recv [this] (.recv this #?(:clj Long/MAX_VALUE :clje :infinity)))
  (recv [_this timeout] (recv-fn timeout))
  #?(:clj java.io.Closeable :clje erlang.io.ICloseable)
  (close [_this] (close))
  ;; A clojerl deftype has no default hash or equality, and the server
  ;; keeps transports in a set.
  #?@(:clje [clojerl.IHash
             (hash [this] (erlang/phash2 this))
             clojerl.IEquiv
             (equiv [this other] (erlang/=:= this other))
             clojerl.IStringable
             (str [_] "#<nrepl.transport.FnTransport>")]))

(#?(:clj defn :clje comment) fn-transport
  "Returns a Transport implementation that delegates its functionality
   to the 2 or 3 functions provided."
  ([transport-read write] (fn-transport transport-read write nil))
  ([transport-read write close]
   (let [read-queue (SynchronousQueue.)
         fut (threading/run-with @threading/transport-executor
               (while (not (Thread/interrupted))
                 (try
                   (.put read-queue (transport-read))
                   (catch InterruptedException _
                     ;; Interrupted flag will end the loop.
                     (.put read-queue nil))
                   (catch Throwable t
                     (.put read-queue t)
                     (.interrupt (Thread/currentThread))))))
         transport-promise (promise)
         transport (->FnTransport
                    (let [failure (atom nil)]
                      #(if @failure
                         (throw @failure)
                         (let [msg (.poll read-queue % TimeUnit/MILLISECONDS)]
                           (if (instance? Throwable msg)
                             (do (reset! failure msg)
                                 (.cancel fut true)
                                 (throw msg))
                             msg))))
                    write
                    (fn []
                      (ignore-exceptions (close))
                      (ignore-exceptions (.cancel fut true))
                      (ignore-exceptions
                       (session-util/close-transport-sessions @transport-promise))))]
     (deliver transport-promise transport)
     transport)))

#?(:clje
   (defn fn-transport
     "Returns a Transport implementation that delegates its functionality
   to the 2 or 3 functions provided."
     ([transport-read write] (fn-transport transport-read write nil))
     ([transport-read write close]
      ;; A reader process takes the place of the reader thread. It hands each
      ;; message over a synchronous queue, as the JVM version does.
      (let [read-queue (queue/start true)
            reader (threading/run-with @threading/transport-executor
                     (loop []
                       (let [[ok? v] (try [true (transport-read)]
                                          (catch _ t [false t]))]
                         (queue/put read-queue (if ok? v #erl[::failure v]))
                         (when ok? (recur)))))
            failure? #(and (erlang/is_tuple %) (= ::failure (erlang/element 1 %)))
            transport-promise (promise)
            transport (->FnTransport
                       (let [failure (atom nil)]
                         #(if @failure
                            (throw @failure)
                            (let [msg (queue/poll read-queue %)]
                              (if (failure? msg)
                                (do (reset! failure (erlang/element 2 msg))
                                    (erlang/exit reader :kill)
                                    (throw @failure))
                                msg))))
                       write
                       (fn []
                         (ignore-exceptions (close))
                         (erlang/exit reader :kill)
                         (queue/close read-queue)
                         (ignore-exceptions
                          (session-util/close-transport-sessions @transport-promise))))]
        (deliver transport-promise transport)
        transport))))

#?(:clje
   (defn- disconnected-error
     "Return the error for a lost connection. The JVM throws a
  SocketException, which the BEAM does not have."
     []
     (ex-info "The transport's socket appears to have lost its connection to the nREPL server"
              {::disconnected true})))

#?(:clje
   (defn disconnected?
     "Return true if the error `e` reports a lost transport connection."
     [e]
     (boolean (::disconnected (ex-data e)))))

#?(:clje
   (defn- disconnection?
     "Return true if the error `e` from a read or write on `s` means that
  the connection is lost."
     [e s]
     (or (beam-socket/disconnected? e)
         (::bencode/eof (ex-data e))
         (and (satisfies? socket/Connectable s) (not (socket/is-connected? s))))))

#?(:clje
   (defmacro ^:private rethrow-on-disconnection
     {:style/indent 1}
     [s & body]
     `(try
        ~@body
        (catch ~'_ e#
          (if (disconnection? e# ~s)
            (throw (disconnected-error))
            (throw e#))))))

(#?(:clj defmacro :clje comment) ^:private rethrow-on-disconnection
  {:style/indent 1}
  [s & body]
  `(try
     ~@body
     (catch ClosedChannelException e#
       (throw (SocketException. "The transport's socket appears to have lost its connection to the nREPL server")))
     (catch RuntimeException e#
       (if (= "EOF while reading" (.getMessage e#))
         (throw (SocketException. "The transport's socket appears to have lost its connection to the nREPL server"))
         (throw e#)))
     (catch EOFException e#
       (if (= "Invalid netstring. Unexpected end of input." (.getMessage e#))
         (throw (SocketException. "The transport's socket appears to have lost its connection to the nREPL server"))
         (throw e#)))
     (catch Throwable e#
       (if (let [s# ~s]
             (and (satisfies? socket/Connectable s#) (not (socket/is-connected? s#))))
         (throw (SocketException. "The transport's socket appears to have lost its connection to the nREPL server"))
         (throw e#)))))

(defn- safe-write-bencode
  "Similar to `bencode/write-bencode`, except it will only writes to the output
   stream if the whole `thing` is writable. In practice, it avoids sending partial
    messages down the transport, which is almost always bad news for the client.

   This will still throw an exception if called with something unencodable."
  [output thing]
  (let [buffer #?(:clj (ByteArrayOutputStream.) :clje (bytes/binary-output))]
    (bencode/write-bencode buffer thing)
    (socket/write output #?(:clj (.toByteArray buffer) :clje (bytes/output->binary buffer)))))

(defn- parse-bencode-payload
  "Read the object received from Bencode transport, parse it into
  Clojure data structures, and perform the following transforms:
  - Convert string keys in maps to keywords.
  - For keys that end with \"?\", convert empty lists to nil (as per Lisp convention).
  - Skip decoding values for the list of keys specified in key \"-unencoded\"."
  [payload]
  (let [->string #?(:clj #(if (bytes? %) (String. ^bytes % "UTF-8") %)
                   ;; On the BEAM, bencode strings are already binaries.
                    :clje identity)
        unencoded-keys (some->> (get payload "-unencoded") (map ->string) set)
        walk (fn walk [obj top?]
               (cond (map? obj)
                     (reduce-kv
                      (fn [acc k v]
                        (assoc acc (keyword k)
                               (cond (and top? (contains? unencoded-keys k)) v
                                     (and (str/ends-with? k "?") (= v ())) nil
                                     :else (walk v false))))
                      {} obj)

                     (sequential? obj) (mapv #(walk % false) obj)
                     #?@(:clj [(bytes? obj) (->string obj)])
                     :else obj))]
    (walk payload true)))

(defn bencode
  "Returns a Transport implementation that serializes messages
   over the given Socket or InputStream/OutputStream using bencode."
  ([s] (bencode s s s))
  ([in out & [s]]
   (let [in #?(:clj (PushbackInputStream. (socket/buffered-input in))
               :clje (socket/buffered-input in))
         out (socket/buffered-output out)]
     (fn-transport
      #(rethrow-on-disconnection s
         (parse-bencode-payload (bencode/read-nrepl-message in)))
      #(rethrow-on-disconnection s
         (locking out
           (safe-write-bencode out %)
           #?(:clj (.flush ^Flushable out))))
      (fn []
        (if s
          (.close ^#?(:clj Closeable :clje erlang.io.ICloseable) s)
          (do
            (.close ^#?(:clj Closeable :clje erlang.io.ICloseable) in)
            (.close ^#?(:clj Closeable :clje erlang.io.ICloseable) out))))))))

(defn edn
  "Returns a Transport implementation that serializes messages
   over the given Socket or InputStream/OutputStream using EDN."
  {:added "0.7"}
  ([s] (edn s s s))
  ([in out & [s]]
   (let [in #?(:clj (java.io.PushbackReader. (io/reader (socket/buffered-input in)))
               :clje (bytes/char-reader (socket/buffered-input in)))
         out (socket/buffered-output out)]
     (fn-transport
      #(rethrow-on-disconnection s
         #?(:clj (edn/read in)
            ;; clojerl reports the end of input with an error that carries
            ;; no type, so read with an eof value instead.
            :clje (let [v (edn/read {:eof ::eof} in)]
                    (if (= ::eof v) (throw (disconnected-error)) v))))
      #(rethrow-on-disconnection s
         (locking out
           (binding [*print-readably* true
                     *print-length*   nil
                     *print-level*    nil]
             (socket/write out #?(:clj (.getBytes ^String (str %) "UTF-8") :clje (str %)))
             #?(:clj (.flush ^Flushable out)))))
      (fn []
        (if s
          (.close ^#?(:clj Closeable :clje erlang.io.ICloseable) s)
          (do
            (.close ^#?(:clj Closeable :clje erlang.io.ICloseable) in)
            (.close ^#?(:clj Closeable :clje erlang.io.ICloseable) out))))))))

(#?(:clj defn :clje comment) tty
  "Returns a Transport implementation suitable for serving an nREPL backend
   via simple in/out readers, as with a tty or telnet connection."
  ([s] (tty s s s))
  ([in out & [^Closeable s]]
   (let [r (LineNumberingPushbackReader. (io/reader in))
         w (io/writer out)
         cns (atom "user")
         read-sync (Semaphore. 1)
         eval-ids (atom #{})
         session-id (atom nil)
         read-queue (LinkedBlockingQueue.)
         read #(or (.poll read-queue)
                   (let [id (str "eval" (uuid))]
                     (.acquire read-sync)
                     (swap! eval-ids conj id)
                     {:op "eval", :code r, :ns @cns, :id id, :session @session-id}))
         write (fn [{:keys [out err value status ns new-session id]}]
                 (when new-session (reset! session-id new-session))
                 (when ns (reset! cns ns))
                 (doseq [^String x [out err value] :when x]
                   (.write w x))
                 (when (and (some #{:done "done"} status) (contains? @eval-ids id))
                   (swap! eval-ids disj id)
                   (.write w (str "\n" @cns "=> "))
                   (.release read-sync))
                 (.flush w))
         close (when s
                 #(do (.put read-queue {:op "close", :session @session-id})
                      (.close s)))]
     (.put read-queue {:op "clone"})
     (fn-transport read write close))))

#?(:clje
   (defn tty
     "Returns a Transport implementation suitable for serving an nREPL backend
   via simple in/out readers, as with a tty or telnet connection."
     ([s] (tty s s s))
     ([in out & [s]]
      ;; The eval reads the code from `r` in the session process. A queue
      ;; that holds one token takes the place of the JVM semaphore.
      (let [r (bytes/char-reader (socket/buffered-input in))
            w (socket/buffered-output out)
            cns (atom "user")
            read-sync (doto (queue/start false) (queue/put :token))
            eval-ids (atom #{})
            session-id (atom nil)
            read-queue (queue/start false)
            read #(or (queue/poll read-queue 0)
                      (let [id (str "eval" (uuid))]
                        (queue/take read-sync)
                        (swap! eval-ids conj id)
                        {:op "eval", :code r, :ns @cns, :id id, :session @session-id}))
            write (fn [{:keys [out err value status ns new-session id]}]
                    (when new-session (reset! session-id new-session))
                    (when ns (reset! cns ns))
                    (doseq [x [out err value] :when x]
                      (socket/write w x))
                    (when (and (some #{:done "done"} status) (contains? @eval-ids id))
                      (swap! eval-ids disj id)
                      (socket/write w (str "\n" @cns "=> "))
                      (queue/put read-sync :token)))
            close (when s
                    #(do (queue/put read-queue {:op "close", :session @session-id})
                         (.close s)))]
        (queue/put read-queue {:op "clone"})
        (fn-transport read write close)))))

(defn tty-greeting
  "A greeting fn usable with `nrepl.server/start-server`,
   meant to be used in conjunction with Transports returned by the
   `tty` function.

   Usually, Clojure-aware client-side tooling would provide this upon connecting
   to the server, but telnet et al. isn't that."
  [transport]
  (send transport {:out (str ";; nREPL " (:version-string nrepl.version/version)
                             \newline
                             ";; Clojure " (clojure-version)
                             \newline
                             "user=> ")}))

(defmulti uri-scheme
  "Return the uri scheme associated with a transport var."
  identity)

(defmethod uri-scheme #'bencode [_] "nrepl")

(defmethod uri-scheme #'tty [_] "telnet")

(defmethod uri-scheme #'edn [_] "nrepl+edn")

(defmethod uri-scheme :default
  [transport]
  (printf "WARNING: No uri scheme associated with transport %s\n" transport)
  "unknown")

(#?(:clj deftype :clje comment) QueueTransport [^BlockingQueue in ^BlockingQueue out]
  nrepl.transport.Transport
  (send [this msg] (.put out msg) this)
  (recv [_this] (.take in))
  (recv [_this timeout] (.poll in timeout TimeUnit/MILLISECONDS)))

#?(:clje
   (deftype QueueTransport [in out]
     Transport
     (send [this msg] (queue/put out msg) this)
     (recv [_this] (queue/take in))
     (recv [_this timeout] (queue/poll in timeout))

     clojerl.IStringable
     (str [_] "#<nrepl.transport.QueueTransport>")))

(defn piped-transports
  "Returns a pair of Transports that read from and write to each other."
  []
  (let [a #?(:clj (LinkedBlockingQueue.) :clje (queue/start false))
        b #?(:clj (LinkedBlockingQueue.) :clje (queue/start false))]
    [(QueueTransport. a b) (QueueTransport. b a)]))

(defn respond-to
  "Send a response for `msg` with `response-data` using message's transport."
  [msg & response-data]
  (send (:transport msg) (apply response-for msg response-data)))

(defmacro safe-handle
  "When given a message and a number of <op handler> pairs, invoke the handler
  with the op that matches message's op. If an exception is raised during
  handling, send an automatic error response through the message's transport
  with `:<op>-error` status. Special keyword `:else` can be used for an op to
  define a catch-all handler. Handlers should be functions of 1 argument `msg`."
  {:style/indent 1}
  [msg & body] ;; `body` is used, otherwise CIDER acts up and misindents
  (assert (even? (count body)))
  (let [msg-sym (gensym "msg")
        op-sym (gensym "op")]
    `(let [~msg-sym ~msg
           ~op-sym (:op ~msg-sym)]
       (cond ~@(mapcat (fn [[op handler]]
                         (if (= op :else)
                           [:else `(~handler ~msg-sym)]
                           [`(= ~op-sym ~op)
                            `(try (respond-to ~msg-sym (~handler ~msg-sym))
                                  (catch #?(:clj Exception :clje ~'_) e#
                                    (respond-to ~msg-sym
                                                :status #{~(keyword (str op "-error")) :error :done}
                                                :message #?(:clj (.getMessage e#)
                                                            :clje (or (ex-message e#) (pr-str e#))))))]))
                       (partition 2 body))))))

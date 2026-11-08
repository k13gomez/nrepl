(ns nrepl.socket
  "Compatibility layer for java.io vs java.nio sockets to allow an
  incremental transition to nio, since the JDK's filesystem sockets
  don't support the java.io socket interface, and we can't use the
  compatibility layer for bidirectional read and write:
  https://bugs.openjdk.java.net/browse/JDK-4509080."
  (:require
   #?(:clj [clojure.java.io :as io])
   [nrepl.misc :refer [log]]
   [nrepl.tls :as tls]
   #?@(:clje [[nrepl.beam.bytes :as bytes]
              [nrepl.beam.compat :refer [requiring-resolve]]
              [nrepl.beam.socket :as beam-socket]]))
  #?(:clj
     (:import
      (java.io BufferedInputStream BufferedOutputStream OutputStream)
      (java.net InetSocketAddress ServerSocket Socket URI)
      (java.nio ByteBuffer)
      (java.nio.channels Channels ClosedChannelException ServerSocketChannel SocketChannel)
      (javax.net.ssl SSLServerSocket))
     :clje
     (:import
      (nrepl.beam.socket Connection ServerSocket))))

;;; InetSockets (TCP)

(defn inet-socket
  ([bind port]
   (let [port (or port 0)
         #?@(:clj [addr (fn [^String bind port] (InetSocketAddress. bind (int port)))])
         ;; We fallback to 127.0.0.1 instead of to localhost to avoid
         ;; a dependency on the order of ipv4 and ipv6 records for
         ;; localhost in /etc/hosts
         bind (or bind "127.0.0.1")]
     #?(:clj (doto (ServerSocket.)
               (.setReuseAddress true)
               (.bind (addr bind port)))
        :clje (beam-socket/tcp-server-socket bind port))))
  ([bind port tls-context]
   (let [port (or port 0)
         ;; We fallback to 127.0.0.1 instead of to localhost to avoid
         ;; a dependency on the order of ipv4 and ipv6 records for
         ;; localhost in /etc/hosts
         bind (or bind "127.0.0.1")]
     (tls/server-socket tls-context bind port))))

;; Unix domain sockets

;;;; Compatibility block.
;; Conditionally loads a particular implementation namespace based on
;; availability of classes on classpath.

(defn- throw-no-sockets []
  (let [msg "Support for filesystem sockets requires JDK 17+ or a junixsocket dependency"]
    (log msg)
    (throw (ex-info msg {:nrepl/kind ::no-filesystem-sockets}))))

(def unix-domain-flavor
  #?(:clj (cond
            (resolve 'java.net.UnixDomainSocketAddress) :jdk
            (resolve 'org.newsclub.net.unix.AFUNIXSocketAddress) :junixsocket
            :else nil)
     ;; gen_tcp supports filesystem sockets on every BEAM.
     :clje :beam))

(defn unix-server-socket
  "Returns a server filesystem socket bound to the path based on available
  implementation. Throws ex-info map with {:nrepl/kind ::no-filesystem-sockets}
  if none is available."
  ^#?(:clj ServerSocketChannel :clje erlang.Type) [path]
  (case unix-domain-flavor
    #?@(:clje [:beam (beam-socket/unix-server-socket path)])
    :jdk
    ((requiring-resolve 'nrepl.socket.jdk17/unix-server-socket) path)
    :junixsocket
    ((requiring-resolve 'nrepl.socket.junixsocket/unix-server-socket) path)
    ;; else
    (throw-no-sockets)))

(defn unix-client-socket
  "Returns a client filesystem socket bound to the path based on available
  implementation. Throws ex-info map with {:nrepl/kind ::no-filesystem-sockets}
  if none is available."
  ^#?(:clj SocketChannel :clje erlang.Type) [path]
  (case unix-domain-flavor
    #?@(:clje [:beam (beam-socket/unix-client path)])
    :jdk
    ((requiring-resolve 'nrepl.socket.jdk17/unix-client-socket) path)
    :junixsocket
    ((requiring-resolve 'nrepl.socket.junixsocket/unix-client-socket) path)
    ;; else
    (throw-no-sockets)))

(#?(:clj defn- :clje comment) get-socket-address-path
  "Given a socket address instance, return its filesystem path as a
  string. Depends on loaded Unix socket implementation."
  [addr]
  (case unix-domain-flavor
    :jdk
    ((requiring-resolve 'nrepl.socket.jdk17/get-socket-address-path) addr)
    :junixsocket
    ((requiring-resolve 'nrepl.socket.junixsocket/get-socket-address-path) addr)
    ;; else
    (throw-no-sockets)))

;;;; Common parts.

(#?(:clj defn :clje comment) as-nrepl-uri
  "Constructs an nREPL URI from a server map and transport scheme.
  Takes a map with :server-socket and :host keys (as in nrepl.server/Server)."
  ^URI [{:keys [host server-socket]} transport-scheme]
  (if (instance? ServerSocketChannel server-socket)
    (URI. (str transport-scheme "+unix")
          (get-socket-address-path
           (.getLocalAddress ^ServerSocketChannel server-socket))
          nil)
    (URI. (str transport-scheme
               (when (instance? SSLServerSocket server-socket)
                 "s"))
          nil
          host
          (.getLocalPort ^ServerSocket server-socket)
          nil nil nil)))

#?(:clje
   (defn as-nrepl-uri
     "Constructs an nREPL URI from a server map and transport scheme.
  Takes a map with :server-socket and :host keys (as in nrepl.server/Server).
  The BEAM has no URI type, so it returns the URI as a string."
     [{:keys [host server-socket]} transport-scheme]
     (if-let [path (beam-socket/unix-path server-socket)]
       (str transport-scheme "+unix:" (filename/absname path))
       (str transport-scheme
            (when (beam-socket/tls? server-socket) "s")
            "://" host ":" (beam-socket/local-port server-socket)))))

(defprotocol Acceptable
  (accept [s]
    "Accepts a connection on s.  Throws ClosedChannelException if s is
    closed."))

;; TCP connections get TCP_NODELAY. A reply is usually several small
;; messages written one after another, like an eval's value and then its
;; done, and with Nagle's algorithm each one waits for the client to
;; acknowledge the one before, which Linux delays by up to 40ms.
(#?(:clj extend-protocol :clje comment) Acceptable
  ServerSocketChannel
  (accept [s] (.accept s))

  SSLServerSocket
  (accept [s]
    (when (.isClosed s)
      (throw (ClosedChannelException.)))
    (doto ^Socket (tls/accept s)
      (.setTcpNoDelay true)))

  ServerSocket
  (accept [s]
    (when (.isClosed s)
      (throw (ClosedChannelException.)))
    (doto (.accept s)
      (.setTcpNoDelay true))))

;; Accepted BEAM sockets inherit TCP_NODELAY from the listen socket.
#?(:clje
   (extend-protocol Acceptable
     ServerSocket
     (accept [s]
       (if (beam-socket/tls? s)
         (tls/accept s)
         (beam-socket/accept s)))))

(defprotocol Connectable
  (is-connected? [s]
    "Returns true if socket-like thing `s` is currently connected."))

(extend-protocol Connectable
  #?@(:clj [SocketChannel
            (is-connected? [s] (.isConnected s))

            Socket
            (is-connected? [s] (.isConnected s))]
      :clje [Connection
             (is-connected? [s] (beam-socket/connected? s))]))

;; We have to handle this ourselves for NIO because unfortunately read and write
;; hang if we use both Channels/newInputStream and Channels/newOutputStream.
;; Read and write deadlock on a shared channel input/output stream lock
;; (cf. https://bugs.openjdk.java.net/browse/JDK-4509080).  Verified that this
;; still happens (via thread dump when hung) with jdk 17.

(defprotocol Writable
  ;; Underscores were added to satisfy clj-kondo
  (write
    [w byte-array]
    [w byte-array offset length]
    "Writes the given bytes to the output as per OutputStream write."))

(#?(:clj extend-protocol :clje comment) Writable
  OutputStream
  (write
    ([s byte-array] (.write ^OutputStream s ^"[B" byte-array))
    ([s byte-array offset length]
     (.write ^OutputStream s byte-array offset length))))

#?(:clje
   (extend-protocol Writable
     Connection
     (write
       ([s data] (bytes/write s data))
       ([s data offset length]
        (bytes/write s (erlang/binary_part data offset length))))))

(#?(:clj defrecord :clje comment) BufferedOutputChannel
                                  [^SocketChannel channel ^ByteBuffer buffer]

  java.io.Flushable
  (flush [_this] ;; Underscore was added to satisfy clj-kondo
    (.flip buffer)
    (.write channel buffer)
    (.clear buffer))

  Writable
  (write [this byte-array]
    (.write this byte-array 0 (count byte-array)))
  (write [this byte-array offset length]
    (if (> length (.capacity buffer))
      (do
        (.flush this)
        (.write channel (ByteBuffer/wrap byte-array offset length)))
      (do
        (when (> length (.remaining buffer))
          (.flush this))
        (.put buffer byte-array offset length)))))

(#?(:clj defn :clje comment) buffered-output-channel [^SocketChannel channel bytes]
  (assert (.isBlocking channel))
  (->BufferedOutputChannel channel (ByteBuffer/allocate bytes)))

(defprotocol AsBufferedInputStreamSubset
  (buffered-input [x]
    "Returns a buffered stream (subset of BufferedInputStream) reading from x."))

(extend-protocol AsBufferedInputStreamSubset
  #?@(:clj [;; Use the Channels stream for input but not output to avoid the deadlock
            SocketChannel (buffered-input [s] (-> s Channels/newInputStream io/input-stream))
            Socket (buffered-input [s] (io/input-stream s))
            BufferedInputStream (buffered-input [s] s)]
      ;; A BEAM connection buffers its own input. Any other byte input
      ;; (for example a binary input in tests) is used as it is.
      :clje [default (buffered-input [s] s)]))

(defprotocol AsBufferedOutputStreamSubset
  (buffered-output [x]
    "Returns a buffered stream (subset of BufferedOutputStream) reading from x."))

(extend-protocol AsBufferedOutputStreamSubset
  #?@(:clj [;; Use the Channels stream for input but not output to avoid the deadlock
            SocketChannel (buffered-output [s] (buffered-output-channel s 8192))
            Socket (buffered-output [s] (io/output-stream s))
            BufferedOutputStream (buffered-output [s] s)]
      ;; A BEAM connection writes each message with one send.
      :clje [default (buffered-output [s] s)]))

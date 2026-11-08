(ns nrepl.middleware.interruptible-eval
  "Supports the ability to evaluate code. The name of the middleware is
  slightly misleading, as interrupt is currently supported at a session level
  but the name is retained for backwards compatibility."
  {:author "Chas Emerick"}
  (:refer-clojure :exclude [read])
  (:require
   #?(:clj [clojure.java.io :as io])
   clojure.main
   clojure.test
   [nrepl.middleware :refer [set-descriptor!]]
   [nrepl.middleware.caught :as caught]
   [nrepl.middleware.print :as print]
   [nrepl.transport :as t]
   #?@(:clje [[nrepl.beam.bytes :as bytes]
              [nrepl.beam.compat :as compat]
              nrepl.in]))
  #?(:clj
     (:import
      (clojure.lang Compiler$CompilerException
                    LineNumberingPushbackReader LispReader$ReaderException)
      (java.io PushbackReader StringReader Writer)
      (java.lang.reflect Field))))

(def ^:dynamic *msg*
  "The message currently being evaluated."
  nil)

(#?(:clj defn- :clje comment) set-line!
  [^LineNumberingPushbackReader reader line]
  (-> reader (.setLineNumber line)))

(#?(:clj defn- :clje comment) set-column!
  [^LineNumberingPushbackReader reader column]
  (when-let [field (->> LineNumberingPushbackReader
                        (.getDeclaredFields)
                        (filter #(= "_columnNumber" (.getName ^Field %)))
                        first)]
    (-> ^Field field
        (doto (.setAccessible true))
        (.set reader column))))

(#?(:clj defn- :clje comment) source-logging-pushback-reader
  [code line column]
  (let [reader (LineNumberingPushbackReader. (StringReader. code))]
    (when line (set-line! reader (int line)))
    (when column (set-column! reader (int column)))
    reader))

#?(:clje
   (defn- source-logging-pushback-reader
     "Return a reader over `code` that reports positions from `line` and
  `column`. clojerl readers always start at line 1 and column 1, so the
  code gets that many newlines and spaces in front."
     [code line column]
     (bytes/char-reader
      (bytes/binary-input
       (str (apply str (repeat (dec (or line 1)) "\n"))
            (apply str (repeat (dec (or column 1)) " "))
            code)))))

(#?(:clj defn- :clje comment) interrupted?
  "Returns true if the given throwable was ultimately caused by an interrupt."
  [^Throwable e]
  (or (instance? ThreadDeath (clojure.main/root-cause e))
      (and (instance? Compiler$CompilerException e)
           (instance? ThreadDeath (.getCause e)))))

(defn- short-file-name [source-path]
  (try #?(:clj (.getName (io/file source-path)) :clje (filename/basename source-path))
       (catch #?(:clj Exception :clje _) _)))

(#?(:clj defn- :clje comment) skip-stdin-newline []
  ;; NB: confusing hack. When `(read)` is issued to an input stream, it
  ;; returns a Lisp form, ending with the last character of the form,
  ;; naturally (e.g. a closing paren). However, it is expected that any
  ;; trailing newline after such form is thrown away in order for a
  ;; subsequent `(read-line)` call to not just return that empty newline but
  ;; the actual following content. To simulate this behavior, we run
  ;; this newline-skipping function after every `eval` request.
  ;; Note that we check if stdin is ready in order not to block if it is empty.
  ;; The evaluated code may have set! *in* to some other Reader; the hack only
  ;; makes sense for the line-numbering reader a session binds *in* to.
  (when (and (instance? LineNumberingPushbackReader *in*)
             (.ready ^LineNumberingPushbackReader *in*))
    (clojure.main/skip-if-eol *in*)))

#?(:clje
   (defn- skip-stdin-newline []
     ;; The same hack as the JVM version. clojure.main/skip-if-eol needs an
     ;; erlang.io.PushbackReader, so this does the same with the session reader.
     (when (and (instance? nrepl.in.QueuePollingReader *in*)
                (nrepl.in/ready? *in*))
       (let [c (erlang.io.IReader/read *in*)]
         (when-not (or (= "\n" c) (= :eof c))
           (erlang.io.IPushbackReader/unread *in* c))))))

(#?(:clj defn :clje comment) evaluator
  "Return a closure that evaluates msg's `:code` (either a string or a seq of
  forms to be evaluated) within the dynamic context of its session. `:ns` can be
  optionally specified (resolved via `find-ns`). The message MUST contain a
  Transport implementation in :transport; expression results and errors will be
  sent via that Transport."
  [msg]
  (fn run []
    (let [{:keys [eval ns code file file-name line column]} msg
          ;; Qualified read-fn and eval-fn are a way for middleware to provide
          ;; custom read and eval functions which can be runtime closures.
          ;; `:eval` parameter that can come from a client can only be a symbol
          ;; pointing to a var.
          read-fn (or (::read-fn msg) clojure.core/read)
          eval-fn (or (::eval-fn msg) (some-> eval symbol find-var))
          ;; Needed exclusively by `load-file` middleware to say that if error
          ;; happens anywhere during the evaluation of the file, don't try
          ;; evaluating subsequent forms.
          stop-on-error? (::stop-on-error msg)
          ;; A way to provide extra temporary bindings (not session-persisted).
          bindings (or (::bindings msg) {})
          file-name (or file-name (short-file-name file))
          explicit-ns (some-> ns symbol find-ns)
          eof (Object.)
          errored? (volatile! false)
          read-cond (keyword (:read-cond msg :allow))
          read (cond (string? code)
                     (let [reader (source-logging-pushback-reader code line column)]
                       #(read-fn {:read-cond read-cond :eof eof} reader))

                     ;; Special case, only used for TTY transport.
                     (instance? PushbackReader code)
                     (let [already-read? (volatile! false)]
                       ;; For TTY, only allow one read per eval, then
                       ;; return EOF.
                       #(if @already-read?
                          eof
                          (do (vreset! already-read? true)
                              (read-fn {:read-cond read-cond :eof eof} code))))

                     (instance? Iterable code)
                     (let [code (.iterator ^Iterable code)]
                       #(if (.hasNext code)
                          (.next code)
                          eof)))
          caught (fn [^Throwable e]
                   (set! *e e)
                   (vreset! errored? true)
                   (when-not (interrupted? e)
                     (t/respond-to msg {::caught/throwable e
                                        :status #{:eval-error}
                                        :ex (str (class e))
                                        :root-ex (str (class (clojure.main/root-cause e)))})))]
      (push-thread-bindings (merge (when explicit-ns {#'*ns* explicit-ns})
                                   (when (and file file-name)
                                     {Compiler/SOURCE_PATH file
                                      Compiler/SOURCE file-name})
                                   bindings))
      (try
        (loop []
          (let [input (try
                        (clojure.main/with-read-known (read))
                        (catch Throwable e
                          (let [e (if (instance? LispReader$ReaderException e)
                                    (ex-info nil {:clojure.error/phase :read-source} e)
                                    e)]
                            ;; If error happens during read phase, call
                            ;; caught-hook but don't continue executing.
                            (caught e)
                            eof)))]
            (when-not (identical? input eof)
              (try
                (let [value (if eval-fn
                              (eval-fn input)
                              ;; If eval-fn is not provided, call
                              ;; Compiler/eval directly for slimmer stack.
                              (Compiler/eval input true))]
                  (set! *3 *2)
                  (set! *2 *1)
                  (set! *1 value)
                  (try
                    ;; *out* has :tag metadata; *err* does not
                    (.flush ^Writer *err*)
                    (.flush *out*)
                    (t/respond-to msg {:ns (str (ns-name *ns*))
                                       :value value
                                       ::print/keys #{:value}})
                    (catch Throwable e
                      (throw (ex-info nil {:clojure.error/phase :print-eval-result} e)))))
                (catch Throwable e
                  (caught e)))
              ;; Otherwise, when errors happen during eval/print phase,
              ;; report the exception but continue executing the
              ;; remaining readable forms, unless we load the whole file.
              (when-not (and stop-on-error? @errored?)
                (recur)))))

        (catch Throwable e
          (caught e))
        (finally
          ;; The evaluated code may have closed *out* or *in* (e.g. `(slurp
          ;; *in*)`). An exception escaping from here would kill the session
          ;; thread, and the client would never see `done`.
          (try
            (flush)
            (skip-stdin-newline)
            (catch Exception _))
          (pop-thread-bindings))))))

#?(:clje
   (defn- run-catching
     "Call `f`. If it throws, call `on-error` with the Erlang class of the
  error (`:error`, `:throw` or `:exit`), the error and its stack trace."
     [f on-error]
     (try
       (f)
       (catch :throw e :stack st (on-error :throw e st))
       (catch :exit e :stack st (on-error :exit e st))
       (catch _ e :stack st (on-error :error e st)))))

#?(:clje
   (defn evaluator
     "Return a closure that evaluates msg's `:code` (either a string or a seq of
  forms to be evaluated) within the dynamic context of its session. `:ns` can be
  optionally specified (resolved via `find-ns`). The message MUST contain a
  Transport implementation in :transport; expression results and errors will be
  sent via that Transport."
     [msg]
     (fn run []
       (let [{:keys [eval ns code file line column]} msg
             read-fn (or (::read-fn msg) clojure.core/read)
             eval-fn (or (::eval-fn msg) (some-> eval symbol find-var))
             stop-on-error? (::stop-on-error msg)
             bindings (or (::bindings msg) {})
             explicit-ns (some-> ns symbol find-ns)
             eof (erlang/make_ref)
             errored? (process-val! false)
             read-cond (keyword (:read-cond msg :allow))
             read-opts (cond-> {:read-cond read-cond :eof eof}
                         file (assoc :file file))
             read (cond (string? code)
                        (let [reader (source-logging-pushback-reader code line column)]
                          #(read-fn read-opts reader))

                        ;; Special case, only used for TTY transport.
                        (satisfies? erlang.io.IPushbackReader code)
                        (let [already-read? (process-val! false)]
                          ;; For TTY, only allow one read per eval, then
                          ;; return EOF.
                          #(if @already-read?
                             eof
                             (do (vreset! already-read? true)
                                 (read-fn read-opts code))))

                        (sequential? code)
                        (let [code (process-val! (seq code))]
                          #(if-let [s @code]
                             (do (vreset! code (next s))
                                 (first s))
                             eof)))
             caught (fn [kind e stacktrace]
                      (set! *e e)
                      (set! *stacktrace stacktrace)
                      (vreset! errored? true)
                      ;; An interrupt kills the process, so there is no
                      ;; interrupted error to filter out here.
                      (t/respond-to msg {::caught/throwable e
                                         :status #{:eval-error}
                                         :ex (str "class " (compat/error-name kind e))
                                         :root-ex (str "class " (compat/error-name kind (compat/root-cause e)))}))]
         (push-thread-bindings (merge (when explicit-ns {#'*ns* explicit-ns})
                                      bindings))
         (compat/restore-ns-mappings! *ns*)
         (try
           (loop []
             (let [input (run-catching
                          #(clojure.main/with-read-known (read))
                          (fn [kind e st]
                            ;; If error happens during read phase, call
                            ;; caught-hook but don't continue executing.
                            (caught kind (ex-info nil {:clojure.error/phase :read-source} e) st)
                            eof))]
               (when-not (= input eof)
                 (run-catching
                  #(let [value (if eval-fn (eval-fn input) (clojure.core/eval input))]
                     (set! *3 *2)
                     (set! *2 *1)
                     (set! *1 value)
                     (run-catching
                      (fn []
                        (erlang.io.IWriter/write *err* "")
                        (erlang.io.IWriter/write *out* "")
                        (t/respond-to msg {:ns (str (ns-name *ns*))
                                           :value value
                                           ::print/keys #{:value}}))
                      (fn [_kind e _st]
                        (throw (ex-info nil {:clojure.error/phase :print-eval-result} e)))))
                  caught)
                 ;; Otherwise, when errors happen during eval/print phase,
                 ;; report the exception but continue executing the
                 ;; remaining readable forms, unless we load the whole file.
                 (when-not (and stop-on-error? @errored?)
                   (recur)))))
           (finally
             ;; The evaluated code may have closed *out* or *in* (e.g. `(slurp
             ;; *in*)`). An exception escaping from here would kill the session
             ;; process, and the client would never see `done`.
             (try
               (flush)
               (skip-stdin-newline)
               (catch _ _))
             (.destroy errored?)
             (pop-thread-bindings)))))))

(defn interruptible-eval
  "Evaluation middleware that supports interrupts.  Returns a handler that supports
   \"eval\" and \"interrupt\" :op-erations that delegates to the given handler
   otherwise."
  [h & _configuration]
  (fn [{:keys [op session id ns code] :as msg}]
    (let [{:keys [exec]} (meta session)]
      (if (= op "eval")
        (cond (nil? code)
              (t/respond-to msg :status #{:error :no-code :done})

              (not (or (string? code)
                       #?(:clj (instance? Iterable code) :clje (sequential? code))
                       #?(:clj (instance? PushbackReader code)
                          :clje (satisfies? erlang.io.IPushbackReader code))))
              (t/respond-to msg :status #{:error :unknown-code-type :done})

              (and (some? ns) (nil? (some-> ns symbol find-ns)))
              (t/respond-to msg {:status #{:error :namespace-not-found :done}
                                 :ns     ns})

              :else (exec id
                          (evaluator msg)
                          #(t/respond-to msg :status :done)
                          msg))
        (h msg)))))

(set-descriptor! #'interruptible-eval
                 {:requires #{"clone" "close" #'caught/wrap-caught #'print/wrap-print}
                  :expects #{}
                  :handles {"eval"
                            {:doc "Evaluates code. Note that unlike regular stream-based Clojure REPLs, nREPL's `\"eval\"` short-circuits on first read error and will not try to read and execute the remaining code in the message."
                             :requires {"code" "The code to be evaluated."
                                        "session" "The ID of the session within which to evaluate the code."}
                             :optional (merge caught/wrap-caught-optional-arguments
                                              print/wrap-print-optional-arguments
                                              {"id" "An opaque message ID that will be included in responses related to the evaluation, and which may be used to restrict the scope of a later \"interrupt\" operation."
                                               "eval" "A fully-qualified symbol naming a var whose function value will be used to evaluate [code], instead of `clojure.core/eval` (the default)."
                                               "ns" "The namespace in which to perform the evaluation. The supplied namespace must exist already (e.g. be loaded). If no namespace is specified the evaluation falls back to `*ns*` for the session in question."
                                               "file" "The path to the file containing [code]. `clojure.core/*file*` will be bound to this."
                                               "file-name" "Name of source file, e.g. io.clj. Do not confuse with `file` param which contains full path while `file-name` is only the short file name. Both parameters are needed to properly set the bytecode location metadata."
                                               "line" "The line number in [file] at which [code] starts."
                                               "column" "The column number in [file] at which [code] starts."
                                               "out-quota" "Maximum number of bytes printed into *out* by this evaluation."
                                               "err-quota" "Maximum number of bytes printed into *err* by this evaluation."
                                               "read-cond" "The options passed to the reader before the evaluation. Useful when middleware in a higher layer wants to process reader conditionals."})
                             :returns {"ns" "*ns*, after successful evaluation of `code`."
                                       "value" "The result of evaluating `code`, often `read`able. This printing is provided by the `print` middleware. Superseded by `ex` and `root-ex` if an exception occurs during evaluation."
                                       "ex" "The type of exception thrown, if any. If present, then `:value` will be absent."
                                       "root-ex" "The type of the root exception thrown, if any. If present, then `:value` will be absent."}}}})

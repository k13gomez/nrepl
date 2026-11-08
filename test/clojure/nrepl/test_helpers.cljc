(ns nrepl.test-helpers
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [nrepl.core :as nrepl]
            [matcher-combinators.test])
  #?(:clj
     (:import (com.hypirion.io ClosingPipe Pipe)
              (java.io File)
              (java.net ServerSocket))))

(def sys-newline
  "The system's newline character sequence."
  #?(:clj (System/lineSeparator) :clje "\n"))

(def win?
  "Whether we are running on MS-Windows."
  #?(:clj (.startsWith (System/getProperty "os.name") "Windows")
     :clje (= :win32 (erlang/element 1 (os/type)))))

(defn string=
  "Like `clojure.core/=` applied on STRING1 and STRING2, but treats
  all line endings as equal."
  [string1 string2]
  (= (str/split-lines string1) (str/split-lines string2)))

(defn newline->sys
  "Returns TEXT, converting `\n` line endings to those of the underlying
  system's, if different."
  [text]
  (if-not (= sys-newline "\n")
    (str/replace text "\n" sys-newline)
    text))

(def sys-file-sep
  "The character separating components in a path."
  #?(:clj File/separator :clje "/"))

(defn dir-sep->sys
  "Replace the `/` separator component used in tests with the one used
  by the system."
  [path]
  (if-not (= sys-file-sep "/")
    (str/replace path "/" sys-file-sep)
    path))

(#?(:clj defn :clje comment) sh
  "A version of clojure.java.shell/sh that streams in/out/err.
  Taken and edited from https://github.com/technomancy/leiningen/blob/f7e1adad6ff5137d6ea56bc429c3b620c6f84128/leiningen-core/src/leiningen/core/eval.clj"
  ^Process
  [cmd]
  (let [proc (.exec (Runtime/getRuntime) ^"[Ljava.lang.String;" (into-array String cmd))]
    (future (with-open [out (.getInputStream proc)
                        err (.getErrorStream proc)
                        in (.getOutputStream proc)]
              (let [_pump-out (doto (Pipe. out System/out) .start)
                    _pump-err (doto (Pipe. err System/err) .start)
                    _pump-in (ClosingPipe. System/in in)]
                (.waitFor proc))))
    proc))

(#?(:clj defmacro :clje comment) with-process [[binding cmd-vector] & body]
  (let [sym (gensym)]
    `(let [~sym (sh ~cmd-vector)
           ~binding ~sym]
       (try ~@body
            (finally (.destroy ~binding))))))

(#?(:clj defmacro :clje comment) with-timeout
  "Runs BODY on another thread and gives up after MS milliseconds, throwing
  instead of hanging the whole test run. For code that blocks on responses
  with no timeout of its own, like the built-in client."
  [ms & body]
  `(let [f# (future ~@body)
         v# (deref f# ~ms ::timeout)]
     (if (identical? v# ::timeout)
       (do (future-cancel f#)
           (throw (ex-info (str "Timed out after " ~ms "ms") {:timeout-ms ~ms})))
       v#)))

#?(:clje
   (defmacro with-timeout
     "Runs BODY in another process and gives up after MS milliseconds, throwing
  instead of hanging the whole test run."
     [ms & body]
     `(let [parent# (erlang/self)
            ref# (erlang/make_ref)
            pid# (erlang/spawn (fn []
                                 (erlang/send parent#
                                              (try #erl[ref# :ok (do ~@body)]
                                                   (catch ~'_ e# #erl[ref# :error e#])))))]
        (receive*
         #erl[ref# :ok v#] v#
         #erl[ref# :error e#] (throw e#)
         (~'after ~ms
                  (erlang/exit pid# :kill)
                  (throw (ex-info (str "Timed out after " ~ms "ms") {:timeout-ms ~ms})))))))

(defn wait-for
  "Poll `pred` every 50 ms until it returns a truthy value or nil if
  `timeout-ms` (default: 5000) elapses."
  ([pred] (wait-for pred 5000))
  ([pred timeout-ms]
   (let [deadline (+ #?(:clj (System/currentTimeMillis) :clje (erlang/system_time :millisecond)) timeout-ms)]
     (loop []
       (or (pred)
           (when (< #?(:clj (System/currentTimeMillis) :clje (erlang/system_time :millisecond)) deadline)
             #?(:clj (Thread/sleep 50) :clje (timer/sleep 50))
             (recur)))))))

(#?(:clj defn :clje comment) free-port []
  (let [sock (ServerSocket. 0)
        port (.getLocalPort sock)]
    (.close sock)
    port))

#?(:clje
   (defn free-port []
     (let [sock (erlang/element 2 (gen_tcp/listen 0 #erl()))
           port (erlang/element 2 (inet/port sock))]
       (gen_tcp/close sock)
       port)))

;; nrepl-specific helpers

(defn eval-value1
  "Transform `code-form` into a string, send to `client-or-session` via `eval` op,
  read and combine the response and return the first successful value."
  [client-or-session code-form]
  (-> (nrepl/message client-or-session {:op "eval", :code (nrepl/code* code-form)})
      nrepl/response-values
      first))

;; matcher-combinators.test is needed for `match?`

(defmacro is+
  "Like `is` but wraps expected value in matcher-combinators's `match?`."
  ([expected actual]
   `(is+ ~expected ~actual nil))
  ([expected actual message]
   ;; The BEAM match? is a function, so call it by its full name.
   `(is (~#?(:clj 'match? :clje 'matcher-combinators.test/match?) ~expected ~actual) ~message)))

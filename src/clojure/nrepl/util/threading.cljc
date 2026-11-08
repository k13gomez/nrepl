(ns nrepl.util.threading
  "Functions and tools for dealing with all threads and threadpools necessary for
  nREPL operation."
  {:added "1.3"}
  (:require
   [nrepl.misc :as misc]
   #?(:clj [nrepl.util.classloader :as classloader]))
  #?(:clj
     (:import
      (java.lang.reflect Method)
      (java.util.concurrent Executors ExecutorService ScheduledExecutorService
                            TimeUnit)
      (nrepl DaemonThreadFactory))))

(#?(:clj defn :clje comment) thread-factory
  "Return a thread factory that produces daemon threads with the name specified by
  `thread-name-fmt`."
  [thread-name-fmt]
  (DaemonThreadFactory. thread-name-fmt (classloader/dynamic-classloader)))

(defmacro run-with
  "Run the provided `body` using `executor`. Don't wait for the result (unless the
  executor itself is blocking)."
  {:style/indent 1}
  [executor & body]
  #?(:clj
     `(.submit ~(with-meta executor {:tag (symbol (.getName ExecutorService))})
               (reify Runnable
                 (run [_#]
                   ~@body)))
     ;; The BEAM runs `body` in a new process. The executor form is not
     ;; evaluated, so the executors below do not exist on clje.
     :clje `(erlang/spawn (fn [] ~@body))))

;; Executors are wrapped in delays to defer their initialization.

(#?(:clj def :clje comment) listen-executor
  "Executor used to accept incoming connections."
  (delay (Executors/newCachedThreadPool (thread-factory "nREPL-server-%d"))))

(#?(:clj def :clje comment) transport-executor
  "Executor used to run the transport handler loop and handle individual requests."
  (delay (Executors/newCachedThreadPool (thread-factory "nREPL-transport-%d"))))

(#?(:clj def :clje comment) handle-executor
  "Executor used to run the handler loop and handle individual requests."
  (delay (Executors/newCachedThreadPool (thread-factory "nREPL-handler-%d"))))

;;;; Thread interruption. Used by session middleware to make eval interruptible.

(#?(:clj def :clje comment) thread-reaper-executor
  "Executor used to kill session threads that did not respond to interrupt."
  (delay (Executors/newScheduledThreadPool
          0 (thread-factory "nREPL-thread-reaper-%d"))))

(#?(:clj defn- :clje comment) deprecated-thread-stop [t]
  (try
    ;; JDK26+ has removed Thread.stop completely, so to avoid linter warnings,
    ;; use a roundabout reflection method to invoke it.
    (when-let [m (.getDeclaredMethod Thread "stop" (into-array Class []))]
      (.invoke ^Method m t (object-array 0)))
    (catch NoSuchMethodException _)))

(#?(:clj defn- :clje comment) jvmti-stop-thread [t]
  ((requiring-resolve 'nrepl.util.jvmti/stop-thread) t))

(#?(:clj defn- :clje comment) try-stop-thread [^Thread t]
  (cond
    (<= misc/java-version 19) (deprecated-thread-stop t)
    ;; Since JDK20, Thread.stop() no longer works. We must resort to using
    ;; JVMTI native agent which luckily still supports Stop Thread command.
    ;; Whether this is more dangerous than calling Thread.stop() in earlier
    ;; JDKs is unknown, but assume the worst and never use this if you can't
    ;; take the risk!
    (misc/jvmti-agent-enabled?) (jvmti-stop-thread t)

    (not (misc/attach-self-enabled?))
    (misc/log "Cannot stop thread on JDK21+ without -Djdk.attach.allowAttachSelf"
              "enabled, see https://docs.nrepl.org/nrepl/installation.html#jvmti.")))

(def ^:private force-stop-delay-ms 5000)

(#?(:clj defn :clje comment) interrupt-stop
  "Try to interrupt the thread normally. Asynchronously wait for 5000ms and, if
  the thread didn't terminate itself cleanly, kill the thread using either
  `.stop` or JVMTI agent. This behaviour strikes a balance between allowing a
  thread to respond to an interrupt, but also ensuring we clean up runaway
  processes."
  [^Thread t]
  ;; TODO: make timeouts configurable?
  (.interrupt t)
  (.schedule ^ScheduledExecutorService @thread-reaper-executor
             ^Runnable #(misc/log-exceptions
                         (when-not (= (.getState t) Thread$State/TERMINATED)
                           (try-stop-thread t)))
             ^long force-stop-delay-ms TimeUnit/MILLISECONDS))

#?(:clje
   (defn- compile-child?
     "Return true if `pid` is a clojerl compiler process. An eval runs a
  `def` init and the compile step in such a child of the caller."
     [pid]
     (let [dict (erlang/process_info pid :dictionary)]
       (and (erlang/is_tuple dict)
            (some #(= :$clj_module_compiling (erlang/element 1 %))
                  (erlang/element 2 dict))))))

#?(:clje
   (defn interrupt-stop
     "Kill the process `pid` at once. The BEAM can always stop a process, so
  there is no grace period as on the JVM. First kill the compiler processes
  that `pid` monitors, because they do not die with it."
     [pid]
     (let [monitors (erlang/process_info pid :monitors)]
       (when (erlang/is_tuple monitors)
         (doseq [m (erlang/element 2 monitors)
                 :let [child (erlang/element 2 m)]
                 :when (and (erlang/is_pid child) (compile-child? child))]
           (erlang/exit child :kill))))
     (erlang/exit pid :kill)))

(ns nrepl.util.out
  "Utilities for injecting output-intercepting writers for System.out and err."
  {:added "1.5"
   :author "Oleksandr Yakushev"}
  (:require [nrepl.util.threading :as threading]
            #?(:clje [nrepl.beam.io :as beam-io]))
  #?(:clj
     (:import (java.io OutputStreamWriter PrintStream PrintWriter)
              (nrepl.out CallbackBufferedOutputStream TeeOutputStream))))

;;;; State management

(defonce ^:private original-print-streams (atom {}))
(defonce ^:private callbacks (atom {:out {}, :err {}}))

(defonce ^{:doc "Atom that contains `true` if standard streams have been wrapped
  by nREPL to enable output forwarding."}
  wrapped?
  (atom false))

(#?(:clj defn- :clje comment) capture-original-print-streams
  "Capture the original System.out and err streams if not already captured."
  []
  (let [og-streams @original-print-streams]
    (when (empty? og-streams)
      ;; Only need to try once, if another thread sets then we are good.
      (compare-and-set! original-print-streams og-streams
                        {:out System/out, :err System/err}))))

(defn set-callback
  "Register or update a callback that is invoked when output is written to stream.
  - `stream` - should be `:out` or `:err` to specify which stream to monitor.
  - `key` - a unique identifier for this callback (used for removal).
  - `callback` - a function that takes a string argument - the line or lines
  sent to the output stream. Multiple callbacks can be registered for the same
  stream using different keys."
  [stream key callback]
  (assert (#{:out :err} stream))
  (swap! callbacks assoc-in [stream key] callback))

(defn remove-callback
  "Remove a previously registered callback for a stream.
  - `stream` - either `:out` or `:err`.
  - `key` - a unique identifier that was used when adding the callback."
  [stream key]
  (assert (#{:out :err} stream))
  (swap! callbacks update stream dissoc key))

(defn- run-stream-callbacks
  "Run all registered callbacks for the given stream (`:out` or `:err`). Each
  callback should be run in a separate thread to prevent stalling and errors."
  [stream s]
  (run! #(threading/run-with @threading/transport-executor
           (% s))
        (vals (get @callbacks stream))))

(#?(:clj defn :clje comment) wrap-standard-streams
  "Wraps and substitutes `System/out` and `System/err` to enable output
  interception via callbacks. After calling this function:

  1. All output to System/out and System/err will be teed - it goes to both
     the original destinations and triggers any registered callbacks.
  2. The root bindings of *out* and *err* are updated to use the wrapped streams.
  3. clojure.test/*test-out* is also rebound if clojure.test is loaded.

  The wrapping is idempotent - calling this function multiple times has no
  additional effect after the first call. This should typically called once
  during nREPL server initialization to enable output capture for all evaluation
  contexts.

  Note: This makes global changes to the JVM's standard streams that affect
  all threads and code running in the same JVM process."
  []
  (letfn [(wrap-stream [stream callback]
            (PrintStream.
             (TeeOutputStream. stream
                               (CallbackBufferedOutputStream. callback 1024))))]
    (when-not @wrapped?
      (reset! wrapped? true)
      (capture-original-print-streams)
      (let [{:keys [out err]} @original-print-streams
            new-out (wrap-stream out (fn global-out-callback [s]
                                       (run-stream-callbacks :out s)))
            new-err (wrap-stream err (fn global-err-callback [s]
                                       (run-stream-callbacks :err s)))]
        ;; Replace stdout/stderr with wrapped versions. This is a global change.
        (System/setOut new-out)
        (System/setErr new-err)
        ;; Also, replace global (root) *out* and *err* bindings. While we take
        ;; care of proper *out*/*err* bindings within the evaluation context (in
        ;; threads that run within the nREPL "session"), threads that spawn
        ;; earlier or through plain Thread. constructor will observe the root
        ;; binding. These root writers don't have their own callback-invoking
        ;; logic, they just write into the wrapped streams that we constructed
        ;; before.
        (.bindRoot #'*out* (PrintWriter. (OutputStreamWriter. new-out)))
        (.bindRoot #'*err* (PrintWriter. (OutputStreamWriter. new-err)))
        ;; Let's also rebind clojure.test/*test-out* because it captures *out*
        ;; during load time. If the var doesn't resolve, it means clojure.test
        ;; hasn't loaded yet, and we don't need to rebind it.
        (some-> ^clojure.lang.Var (resolve 'clojure.test/*test-out*)
                (.bindRoot *out*))))))

#?(:clje
   (defn wrap-standard-streams
     "Tee the standard output of the VM to enable output interception via
  callbacks. After calling this function, each process whose group leader is
  the `user` I/O device gets a tee device as its group leader instead. The
  tee writes to the `user` device and triggers the `:out` callbacks.
  Processes that such a process starts later inherit the tee.

  The root binding of *out* is `:standard_io`, which follows the group
  leader, so it needs no change. Standard error is a registered device that
  this cannot replace, so `:err` callbacks never run on the BEAM.

  The wrapping is idempotent. Note: This makes global changes to the group
  leaders of the VM."
     []
     (when-not @wrapped?
       (reset! wrapped? true)
       (let [user (erlang/whereis :user)
             tee (beam-io/tee-device user (fn global-out-callback [s]
                                            (run-stream-callbacks :out s)))]
         (reset! original-print-streams {:out user})
         (doseq [pid (erlang/processes)
                 :let [gl (erlang/process_info pid :group_leader)]
                 :when (and (erlang/is_tuple gl) (= user (erlang/element 2 gl)))]
           (erlang/group_leader tee pid))))))

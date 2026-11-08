(ns nrepl.sanity-test
  (:require
   [clojure.test :refer :all]
   [nrepl.core :as nrepl]
   [nrepl.middleware.print :as print]
   [nrepl.test-helpers :as th]
   [nrepl.transport :as transport :refer [piped-transports]]))

(#?(:clj deftest :clje comment) repl-out-writer
  (let [[local remote] (piped-transports)
        w (print/replying-PrintWriter :out {:transport remote} {})]
    (doto w
      .flush
      (.println "println")
      (.write "abcd")
      (.write (.toCharArray "ef") 0 2)
      (.write "gh" 0 2)
      (.write (.toCharArray "ij"))
      (.write "   klm" 5 1)
      (.write 32)
      .flush)
    (with-open [out (java.io.PrintWriter. w)]
      (binding [*out* out]
        (newline)
        (prn #{})
        (flush)))

    (is (= [(str "println" (System/getProperty "line.separator"))
            "abcdefghijm "
            (th/newline->sys "\n#{}\n")]
           (->> (nrepl/response-seq local 0)
                (map :out))))))

;; On the BEAM a writer takes strings through erlang.io.IWriter, and an
;; empty string is a flush.
#?(:clje
   (deftest repl-out-writer
     (let [[local remote] (piped-transports)
           w (print/replying-PrintWriter :out {:transport remote} {})]
       (doto w
         (erlang.io.IWriter/write "")
         (erlang.io.IWriter/write "println\n")
         (erlang.io.IWriter/write "abcd")
         (erlang.io.IWriter/write "ef")
         (erlang.io.IWriter/write " ")
         (erlang.io.IWriter/write ""))
       (binding [*out* w]
         (newline)
         (prn #{})
         (flush))
       (.close w)
       ;; The JVM test buffers in a PrintWriter until the flush. Here each
       ;; newline sends the text before it.
       (is (= ["println\n"
               "abcdef "
               "\n"
               "#{}\n"]
              (->> (nrepl/response-seq local 0)
                   (map :out)))))))

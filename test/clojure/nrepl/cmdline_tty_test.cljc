(ns nrepl.cmdline-tty-test
  "TTY is not a full-featured transport (e.g. doesn't support ack), so are tested
   separately here."
  (:require #?(:clj [clojure.java.io :as io])
            [clojure.test :refer [deftest is testing]]
            [nrepl.cmdline :as cmd]
            [nrepl.server :as server]
            [nrepl.test-helpers :refer [free-port is+ #?(:clj sh)]]
            [nrepl.transport :as transport]
            #?@(:clje [[clojure.string :as str]
                       [nrepl.beam.bytes :as bytes]
                       [nrepl.beam.socket :as beam-socket]]))
  #?(:clj
     (:import (java.io BufferedReader PrintStream)
              (org.apache.commons.net.telnet TelnetClient))))

(#?(:clj defn- :clje comment) attempt-connection [client host port timeout-ms]
  (loop [n (/ timeout-ms 500)]
    (or (try
          (.connect ^TelnetClient client ^String host ^int port)
          client
          (catch java.net.ConnectException ex
            (when-not (pos? n)
              (throw ex))))
        (do
          (Thread/sleep 500)
          (recur (dec n))))))

(#?(:clj deftest :clje comment) ^:slow tty-server
  (let [port (free-port)
        server-process (sh ["java" "-Dnreplacktest=y"
                            "-cp" (System/getProperty "java.class.path")
                            "nrepl.main"
                            "--port" (str port)
                            "--transport" "nrepl.transport/tty"])]
    (try
      (let [c (doto (TelnetClient.)
                (attempt-connection "localhost" port 100000)
                ;; A missing reply must fail the test, not hang the run.
                (.setSoTimeout 60000))]
        (with-open [out (PrintStream. (.getOutputStream c))
                    br  (io/reader (.getInputStream c))]
          (doseq [l ["(System/getProperty \"nreplacktest\")"
                     "#?(:clj :clj-form)"
                     "#?(:cljs :cljs-form)"
                     "(clojure.core/require '[clojure.java.io :as io])"
                     "::io/xyz"
                     "(clojure.core/require '[clojure.set :as sets])"
                     "{::io/x 1 ::sets/x 2}"]]
            (.println out l))
          (.flush out)
          (is+ [#"^;; nREPL"
                #"^;; Clojure"
                "user=> \"y\""
                "user=> :clj-form"
                "user=> nil"
                "user=> :clojure.java.io/xyz"
                "user=> nil"
                "user=> {:clojure.java.io/x 1, :clojure.set/x 2}"]
               (vec (repeatedly 8 #(.readLine ^BufferedReader br)))))
        (.disconnect c))
      (finally
        (.destroy ^Process server-process)))))

(deftest no-tty-client
  (testing "Trying to connect with the tty transport should fail."
    (with-open [server (server/start-server :transport-fn #'transport/tty)]
      (let [options (cmd/connection-opts {:port      (:port server)
                                          :host      "localhost"
                                          :transport 'nrepl.transport/tty})]
        (is (thrown? #?(:clj clojure.lang.ExceptionInfo :clje clojerl.ExceptionInfo)
                     (cmd/interactive-repl server options)))))))

;; The BEAM version runs the tty server in this VM and talks to it over a
;; plain TCP connection.

#?(:clje
   (defn- read-line-bytes
     "Read one line from the connection `conn`, without the newline."
     [conn]
     (loop [acc []]
       (let [b (bytes/read-byte conn)]
         (if (or (= -1 b) (= 10 b))
           (erlang/list_to_binary (clj_rt/to_list acc))
           (recur (conj acc b)))))))

#?(:clje
   (deftest tty-server
     (with-open [server (server/start-server :transport-fn #'transport/tty
                                             :greeting-fn (#'cmd/options->greeting {} #'transport/tty))
                 conn (beam-socket/tcp-client "127.0.0.1" (:port server) 10000)]
       (bytes/write conn (str (str/join "\n" ["(+ 1 2)"
                                              "#?(:clje :clje-form)"
                                              "(clojure.core/require '[clojure.string :as s])"
                                              "::s/xyz"
                                              "(clojure.core/require '[clojure.set :as sets])"
                                              "[::s/x ::sets/x]"])
                              "\n"))
       (is+ [#"^;; nREPL"
             #"^;; Clojure"
             "user=> 3"
             "user=> :clje-form"
             "user=> nil"
             "user=> :clojure.string/xyz"
             "user=> nil"
             "user=> [:clojure.string/x :clojure.set/x]"]
            (vec (repeatedly 8 #(read-line-bytes conn)))))))

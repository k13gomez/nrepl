(ns nrepl.transport-test
  (:require [clojure.test :refer [deftest is testing]]
            [matcher-combinators.matchers :as mc]
            [nrepl.test-helpers :refer [is+]]
            [nrepl.transport :as sut]
            #?@(:clje [[nrepl.beam.bytes :as bytes]
                       [nrepl.beam.socket :as beam-socket]]))
  #?(:clj (:import (java.io BufferedInputStream BufferedOutputStream ByteArrayInputStream ByteArrayOutputStream))))

(#?(:clj deftest :clje comment) bencode-safe-write-test
  (testing "safe-write-bencode only writes if the whole message is writable"
    (let [out (ByteArrayOutputStream.)]
      (is (thrown? IllegalArgumentException
                   (#'sut/safe-write-bencode out {"obj" (Object.)})))
      (is (empty? (.toByteArray out))))))

(#?(:clj deftest :clje comment) malformed-bencode-input-test
  (testing "if non-bencode input is passed, throw an informative error"
    (let [in (-> #?(:clj (.getBytes "123456789123456789123456789") :clje "123456789123456789123456789")
                 ByteArrayInputStream.
                 BufferedInputStream.)
          out (BufferedOutputStream. (ByteArrayOutputStream.))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"^nREPL message must be a map."
                            (sut/recv (sut/bencode in out))))))

  (testing "if top-level message is not a map, throw an informative error"
    (let [in (-> #?(:clj (.getBytes "li1ei2ei3ee") :clje "li1ei2ei3ee")
                 ByteArrayInputStream.
                 BufferedInputStream.)
          out (BufferedOutputStream. (ByteArrayOutputStream.))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"^nREPL message must be a map."
                            (sut/recv (sut/bencode in out)))))))

(deftest parse-bencode-test
  (testing "keys are keywordized and byte strings are decoded, at any depth"
    (is (= {:op "eval"
            :code "(+ 1 2)"
            :id 42
            :nested {:list ["a" 1 {:deep "b"}]}
            :nrepl.middleware.print/print "clojure.core/pr"}
           (#'sut/parse-bencode-payload
            {"op" #?(:clj (.getBytes "eval") :clje "eval")
             "code" #?(:clj (.getBytes "(+ 1 2)") :clje "(+ 1 2)")
             "id" 42
             "nested" {"list" [#?(:clj (.getBytes "a") :clje "a") 1 {"deep" #?(:clj (.getBytes "b") :clje "b")}]}
             "nrepl.middleware.print/print" #?(:clj (.getBytes "clojure.core/pr") :clje "clojure.core/pr")})))))

(deftest parse-bencode-question-mark-keys-test
  (testing "empty list under a ?-ending key is treated as nil"
    (is (= {:op "eval"
            :nrepl.middleware.print/stream? nil
            :nrepl.middleware.caught/print? 1
            :outer {:flag? nil}
            :non-empty-value? [1 2 3]
            :doesnt-end-with-q []}
           (#'sut/parse-bencode-payload
            {"op" "eval"
             "nrepl.middleware.print/stream?" []
             "nrepl.middleware.caught/print?" 1
             "outer" {"flag?" []}
             "non-empty-value?" [1 2 3]
             "doesnt-end-with-q" []})))))

(deftest parse-bencode-unencoded-test
  ;; Bytes that are not valid UTF-8, so any decoding would corrupt them.
  (let [binary-data #?(:clj (byte-array [-119 80 78 71 13 10 26 10 0 -1 -2 -128])
                       :clje (erlang/list_to_binary #erl(137 80 78 71 13 10 26 10 0 255 254 128)))]
    (is+ (mc/equals
          {:op "load-file"
           :file "(ns foo)"
           :data #?(:clj bytes? :clje string?)
           :-unencoded ["data"]})
         (#'sut/parse-bencode-payload {"op" #?(:clj (.getBytes "load-file") :clje "load-file")
                                       "file" #?(:clj (.getBytes "(ns foo)") :clje "(ns foo)")
                                       "data" binary-data
                                       "-unencoded" [#?(:clj (.getBytes "data") :clje "data")]}))))

;; The BEAM versions. A binary input keeps its state in the process that
;; made it, and a transport reads in its own process, so the malformed input
;; goes over a loopback socket.

#?(:clje
   (deftest bencode-safe-write-test
     (testing "safe-write-bencode only writes if the whole message is writable"
       (let [out (bytes/binary-output)]
         (is (thrown? clojerl.BadArgumentError
                      (#'sut/safe-write-bencode out {"obj" (erlang/make_ref)})))
         (is (empty? (bytes/output->binary out)))))))

#?(:clje
   (defn- recv-from [input]
     (with-open [server (beam-socket/tcp-server-socket "127.0.0.1" 0)
                 client (beam-socket/tcp-client "127.0.0.1" (beam-socket/local-port server) 1000)]
       (bytes/write client input)
       (with-open [conn (beam-socket/accept server)]
         (sut/recv (sut/bencode conn conn) 1000)))))

#?(:clje
   (deftest malformed-bencode-input-test
     (testing "if non-bencode input is passed, throw an informative error"
       ;; clojerl thrown-with-msg? matches (str e), the whole printed
       ;; error, so the pattern cannot anchor at the start of the message.
       (is (thrown-with-msg? clojerl.ExceptionInfo
                             #"nREPL message must be a map."
                             (recv-from "123456789123456789123456789"))))

     (testing "if top-level message is not a map, throw an informative error"
       (is (thrown-with-msg? clojerl.ExceptionInfo
                             #"nREPL message must be a map."
                             (recv-from "li1ei2ei3ee"))))))

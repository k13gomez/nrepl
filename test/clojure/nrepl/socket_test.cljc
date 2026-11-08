(ns nrepl.socket-test
  (:require [clojure.test :refer [deftest is]]
            [nrepl.socket :as socket]
            #?(:clje [nrepl.beam.socket :as beam-socket]))
  #?(:clj (:import (java.net InetSocketAddress ServerSocket Socket))))

(#?(:clj deftest :clje comment) accept-sets-tcp-nodelay-test
  (with-open [^ServerSocket server (socket/inet-socket "127.0.0.1" 0)
              client (Socket.)]
    (.connect client (InetSocketAddress. "127.0.0.1" (.getLocalPort server)))
    (with-open [^Socket accepted (socket/accept server)]
      (is (.getTcpNoDelay accepted)))))

#?(:clje
   (deftest accept-sets-tcp-nodelay-test
     (with-open [server (socket/inet-socket "127.0.0.1" 0)
                 client (beam-socket/tcp-client "127.0.0.1" (beam-socket/local-port server) 1000)]
       (with-open [accepted (socket/accept server)]
         (is (= #erl[:ok #erl(#erl[:nodelay true])]
                (inet/getopts (.-sock accepted) #erl(:nodelay))))))))

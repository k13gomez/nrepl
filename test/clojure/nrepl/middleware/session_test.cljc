(ns nrepl.middleware.session-test
  {:author "Oleksandr Yakushev"}
  (:require
   [clojure.test :refer :all]
   [matcher-combinators.matchers :as mc]
   [nrepl.config :as config]
   [nrepl.core :as nrepl]
   [nrepl.core-test
    :refer [clean-response repl-server-fixture with-repl-server *server*]]
   nrepl.server
   [nrepl.test-helpers :as th :refer [is+]]
   [nrepl.util.session :as session-util]))

(use-fixtures :each repl-server-fixture)

(def ^:dynamic *test-var1*)
(def ^:dynamic *test-var2* :root)

(#?(:clj deftest :clje comment) dynvar-defaults-test
  (testing "dynvar receives a default value from config"
    (with-redefs [config/config {:dynamic-vars {'nrepl.middleware.session-test/*test-var1* 1}}]
      (with-repl-server
        (let [result (repl-values session "nrepl.middleware.session-test/*test-var1*")]
          (is (= [1] result))))))

  (testing "config can override a dynvar that has a root value"
    (with-repl-server
      (let [result (repl-values session "nrepl.middleware.session-test/*test-var2*")]
        (is (= [:root] result))))

    (with-redefs [config/config {:dynamic-vars {'nrepl.middleware.session-test/*test-var2* :override}}]
      (with-repl-server
        (let [result (repl-values session "nrepl.middleware.session-test/*test-var2*")]
          (is (= [:override] result)))))))

;; clojerl has no with-redefs, and on the BEAM the config comes from
;; `config*`, which caches it. So the BEAM test sets the cache.
#?(:clje
   (defmacro ^:private with-config [config & body]
     `(let [cache# @#'config/config-cache
            old# @cache#]
        (reset! cache# ~config)
        (try ~@body (finally (reset! cache# old#))))))

#?(:clje
   (deftest dynvar-defaults-test
     (testing "dynvar receives a default value from config"
       (with-config {:dynamic-vars {'nrepl.middleware.session-test/*test-var1* 1}}
         (with-repl-server
           (let [result (repl-values session "nrepl.middleware.session-test/*test-var1*")]
             (is (= [1] result))))))

     (testing "config can override a dynvar that has a root value"
       (with-config {}
         (with-repl-server
           (let [result (repl-values session "nrepl.middleware.session-test/*test-var2*")]
             (is (= [:root] result)))))

       (with-config {:dynamic-vars {'nrepl.middleware.session-test/*test-var2* :override}}
         (with-repl-server
           (let [result (repl-values session "nrepl.middleware.session-test/*test-var2*")]
             (is (= [:override] result))))))))

(defn- alive? [sid]
  (some? (session-util/resolve-session sid)))

(defn- session-id [session]
  (:session (::nrepl/taking-until (meta session))))

(def captured-thread (atom nil))

(deftest sessions-closed-with-connection-test
  (reset! captured-thread nil)
  (let [idle-sess (promise)
        busy-sess (promise)]
    (with-repl-server
      (deliver idle-sess (nrepl/client-session client))
      (deliver busy-sess (nrepl/client-session client))
      (repl-eval @busy-sess
                 (nrepl/code
                  (do (reset! nrepl.middleware.session-test/captured-thread
                              #?(:clj (Thread/currentThread) :clje (erlang/self)))
                      #?(:clj (Thread/sleep 30000) :clje (timer/sleep 30000)))))
      (is (th/wait-for #(some? @captured-thread)))
      (is+ (mc/embeds [(session-id @idle-sess)
                       (session-id @busy-sess)])
           (-> (nrepl/message client {:op "ls-sessions"})
               nrepl/combine-responses
               :sessions)))

    (testing "all sessions cloned by the closed connection are closed"
      (is (th/wait-for #(not (or (alive? (session-id @idle-sess))
                                 (alive? (session-id @busy-sess)))))))

    (testing "running evaluation is terminated with the session"
      (is (th/wait-for #(not #?(:clj (.isAlive ^Thread @captured-thread)
                                :clje (erlang/is_process_alive @captured-thread))))))))

(deftest close-op-test
  (with-repl-server
    (let [sid (nrepl/new-session timeout-client)]
      (is+ {:status #{:done :session-closed}}
           (-> (nrepl/message timeout-client {:op "close" :session sid})
               nrepl/combine-responses
               clean-response))
      (is (not (alive? sid)))
      (is+ {:status #{:done :error :unknown-session}}
           (-> (nrepl/message timeout-client {:op "close" :session sid})
               nrepl/combine-responses
               clean-response)))))

(deftest stop-server-closes-sessions-test
  (let [sid (promise)]
    (with-repl-server
      (deliver sid (session-id (nrepl/client-session client)))
      (is (alive? @sid)))
    (nrepl.server/stop-server *server*)
    (is (th/wait-for #(not (alive? @sid))))))

(deftest exception-in-session-loop-test
  (testing "uncaught exception thrown by the exec task kills the session"
    (with-repl-server
      (let [sess (nrepl/client-session client)]
        (is+ {:status #{:done :uncaught-session-exception :error}}
             (-> (nrepl/message sess {:op "eval" :code "(+ 1 2)" :line "bad"})
                 nrepl/combine-responses
                 clean-response))
        (is (not (alive? (session-id sess))))))))

(deftest interrupt-keeps-session-alive-test
  (with-repl-server
    (let [sid (nrepl/new-session client)
          session' (nrepl/client-session client :session sid)
          resp (nrepl/message session' {:op "eval" :code #?(:clj "(Thread/sleep 30000)" :clje "(timer/sleep 30000)")})]
      #?(:clj (Thread/sleep 100) :clje (timer/sleep 100))
      (is+ {:status #{:done}}
           (-> (nrepl/message session' {:op "interrupt"})
               nrepl/combine-responses
               clean-response))
      (is+ {:status #{:done :interrupted}}
           (-> resp nrepl/combine-responses clean-response))
      (is (alive? sid))
      (is (= [3] (nrepl/response-values
                  (nrepl/message session' {:op "eval" :code "(+ 1 2)"})))))))

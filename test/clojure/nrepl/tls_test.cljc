(ns nrepl.tls-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            #?(:clj [com.github.ivarref.locksmith :as locksmith])
            [nrepl.core :as nrepl]
            [nrepl.server :as server]
            [nrepl.socket :as socket]
            [nrepl.test-helpers :refer [eval-value1 win?]]
            #?@(:clj [[nrepl.tls :as tls]
                      [nrepl.tls-client-proxy :as tls-client-proxy]])
            [nrepl.transport :as transport])
  #?(:clj
     (:import (clojure.lang ExceptionInfo)
              (java.lang AutoCloseable)
              (java.net InetSocketAddress Socket)
              (javax.net.ssl SSLException SSLHandshakeException))))

;; The BEAM build has no TLS support, so these tests run only on the JVM.

(#?(:clj defn :clje comment) non-windows-fixture
  "TLS tests are currently broken on Windows, so we use this fixture to disable
  them there instead of messing with test selectors."
  [f]
  (if win?
    (is (= 2 (+ 1 1))) ;; Dummy assertion to make the test succeed.
    (f)))

(#?(:clj use-fixtures :clje comment) :each non-windows-fixture)

(#?(:clj defn :clje comment) gen-key-pair []
  (let [{:keys [ca-cert server-cert server-key client-cert client-key]} (locksmith/gen-certs {:duration-days 1})]
    [(str ca-cert server-cert server-key)
     (str ca-cert client-cert client-key)]))

; Make sure TLS is in fact used
(#?(:clj defn :clje comment) tls-connect
  ^AutoCloseable [opts]
  (#'nrepl/tls-connect opts))

(#?(:clj defn :clje comment) println-err [& args]
  (binding [*out* *err*]
    (apply println args)))

(#?(:clj defn :clje comment) noisy-deref [e]
  (let [dereffed (deref e 3000 :timeout)]
    (if (= :timeout dereffed)
      (do
        (println-err "Timeout when trying to deref exception!" e)
        (is false "Timeout when deref exception!")
        nil)
      dereffed)))

(#?(:clj defn :clje comment) tls-socket-exception? [exception-promise]
  (let [e (noisy-deref exception-promise)]
    (instance? SSLException e)))

(#?(:clj deftest :clje comment) bad-config
  (is (thrown? ExceptionInfo (server/start-server :tls? true)))
  (is (= :nrepl.server/invalid-start-request
         (:nrepl/kind (try
                        (server/start-server :tls? true)
                        (catch Throwable t
                          (ex-data t)))))))

(#?(:clj defn- :clje comment) ctx-error
  "Returns the ExceptionInfo thrown when creating a TLS context from
  `keys-str`, or nil if no exception was thrown."
  ^ExceptionInfo [keys-str]
  (try
    (tls/ssl-context-or-throw keys-str nil)
    nil
    (catch ExceptionInfo e
      e)))

(#?(:clj defn- :clje comment) is-ctx-error
  "Asserts that creating a TLS context from `keys-str` fails with an error
  message containing all of `substrs`."
  [keys-str & substrs]
  (let [e (ctx-error keys-str)]
    (is (some? e))
    (when e
      (doseq [substr substrs]
        (is (str/includes? (.getMessage e) substr))))))

(#?(:clj deftest :clje comment) descriptive-key-material-errors
  (let [{:keys [ca-cert server-cert server-key]} (locksmith/gen-certs {:duration-days 1})]
    (testing "missing private key"
      (is-ctx-error (str ca-cert server-cert) "No private key found"))
    (testing "no certificates"
      (is-ctx-error server-key "No certificates found"))
    (testing "only one certificate"
      (is-ctx-error (str ca-cert server-key) "Only one certificate found"))
    (testing "traditional (non-PKCS#8) private key gets conversion advice"
      (let [sec1-key (str/replace server-key "PRIVATE KEY-----" "EC PRIVATE KEY-----")]
        (is-ctx-error (str ca-cert server-cert sec1-key)
                      "EC PRIVATE KEY format" "openssl pkcs8 -topk8")))
    (testing "an OpenSSH private key is pointed at ssh-keygen, not openssl"
      (let [openssh-key (str/replace server-key "PRIVATE KEY-----" "OPENSSH PRIVATE KEY-----")]
        (is-ctx-error (str ca-cert server-cert openssh-key)
                      "OPENSSH PRIVATE KEY format" "ssh-keygen -p -m PKCS8")))
    (testing "password-protected private key"
      (let [enc-key (str/replace server-key "PRIVATE KEY-----" "ENCRYPTED PRIVATE KEY-----")]
        (is-ctx-error (str ca-cert server-cert enc-key) "password-protected")))
    (testing "an unparsable private key reports the attempted algorithms"
      (let [bad-key "-----BEGIN PRIVATE KEY-----\nAAAA\n-----END PRIVATE KEY-----\n"]
        (is-ctx-error (str ca-cert server-cert bad-key) "Could not parse the private key")))
    (testing "mismatched PEM labels get a structural error instead of \"not found\""
      (let [broken-key (str/replace server-key
                                    "-----END PRIVATE KEY-----"
                                    "-----END RSA PRIVATE KEY-----")]
        (is-ctx-error (str ca-cert server-cert broken-key) "PEM structure")))
    (testing "surrounding whitespace on the PEM fence lines is tolerated"
      (let [indented-key (->> (str/split-lines server-key)
                              (map #(str "  " %))
                              (str/join "\n"))]
        (is (some? (tls/ssl-context-or-throw (str ca-cert server-cert "\n" indented-key "\n") nil)))))
    (testing "the underlying cause is preserved"
      (let [e (ctx-error (str ca-cert server-cert))]
        (is (some? (.getCause e)))))))

(#?(:clj deftest :clje comment) ed25519-private-key-parsing
  (if-let [^java.security.KeyPairGenerator kpg (try
                                                 (java.security.KeyPairGenerator/getInstance "Ed25519")
                                                 (catch java.security.NoSuchAlgorithmException _
                                                   nil))]
    (let [^java.security.PrivateKey private-key (.getPrivate (.generateKeyPair kpg))
          pem (str "-----BEGIN PRIVATE KEY-----\n"
                   (.encodeToString (java.util.Base64/getEncoder) (.getEncoded private-key))
                   "\n-----END PRIVATE KEY-----\n")]
      (is (instance? java.security.PrivateKey (#'tls/str->private-key pem))))
    ;; Ed25519 needs Java 15+; nothing to test on older JDKs.
    (is true)))

(#?(:clj deftest :clje comment) swapped-certificate-order
  ;; The documented keys file layout is CA certificate first, own certificate
  ;; second, but the reverse order should be detected and handled.
  (let [{:keys [ca-cert server-cert server-key client-cert client-key]} (locksmith/gen-certs {:duration-days 1})
        server-keys (str server-cert ca-cert server-key)
        client-keys (str client-cert ca-cert client-key)]
    (with-open [server (server/start-server :tls? true :tls-keys-str server-keys)
                transport (tls-connect {:tls-keys-str client-keys
                                        :host         "127.0.0.1"
                                        :port         (:port server)
                                        :transport-fn transport/bencode})]
      (let [client (nrepl/client transport 30000)]
        (is (= 2 (eval-value1 client '(+ 1 1))))))))

(#?(:clj deftest :clje comment) bad-keys
  (let [[server-keys _] (gen-key-pair)
        [_ client-keys] (gen-key-pair)
        exception (promise)]
    (with-open [server (server/start-server :tls? true
                                            :tls-keys-str server-keys
                                            :consume-exception (partial deliver exception))
                transport (tls-connect {:tls-keys-str client-keys
                                        :host         "127.0.0.1"
                                        :port         (:port server)
                                        :transport-fn (fn [& args]
                                                        ;; there appears to be
                                                        ;; a (race?) condition
                                                        ;; that the
                                                        ;; transport-fn could
                                                        ;; also be called.
                                                        (try
                                                          (apply transport/bencode args)
                                                          (catch Exception e
                                                            (is (instance? SSLHandshakeException e)))))})]
      (let [client (nrepl/client transport 30000)]
        (is (thrown? Exception (eval-value1 client '(+ 1 1))))
        (is (tls-socket-exception? exception))))))

(#?(:clj deftest :clje comment) bad-keys-then-good
  (let [[server-keys good-client-keys] (gen-key-pair)
        [_ bad-client-keys] (gen-key-pair)
        exception (promise)]
    (with-open [server (server/start-server :tls? true
                                            :tls-keys-str server-keys
                                            :consume-exception (partial deliver exception))]
      (with-open [transport (tls-connect {:tls-keys-str bad-client-keys
                                          :host         "127.0.0.1"
                                          :port         (:port server)
                                          :transport-fn (fn [& args]
                                                          ;; there appears to be
                                                          ;; a (race?) condition
                                                          ;; that the
                                                          ;; transport-fn could
                                                          ;; also be called.
                                                          (try
                                                            (apply transport/bencode args)
                                                            (catch Exception e
                                                              (is (instance? SSLHandshakeException e)))))})]
        (let [client (nrepl/client transport 30000)]
          (is (thrown? Exception (eval-value1 client '(+ 1 1))))
          (is (tls-socket-exception? exception))))
      (with-open [transport (tls-connect {:tls-keys-str good-client-keys
                                          :host         "127.0.0.1"
                                          :port         (:port server)
                                          :transport-fn transport/bencode})]
        (let [client (nrepl/client transport 30000)]
          (is (= 2 (eval-value1 client '(+ 1 1)))))))))

(#?(:clj deftest :clje comment) bad-keys-then-good-no-consume-exception
  (let [[server-keys good-client-keys] (gen-key-pair)
        [_ bad-client-keys] (gen-key-pair)]
    (with-open [server (server/start-server :tls? true
                                            :tls-keys-str server-keys)]
      (with-open [transport (tls-connect {:tls-keys-str bad-client-keys
                                          :host         "127.0.0.1"
                                          :port         (:port server)
                                          :transport-fn transport/bencode})]
        (let [client (nrepl/client transport 30000)]
          (is (thrown? Exception (eval-value1 client '(+ 1 1))))))
      (with-open [transport (tls-connect {:tls-keys-str good-client-keys
                                          :host         "127.0.0.1"
                                          :port         (:port server)
                                          :transport-fn transport/bencode})]
        (let [client (nrepl/client transport 30000)]
          (is (= 2 (eval-value1 client '(+ 1 1)))))))))

(#?(:clj deftest :clje comment) conn-close-then-ok
  (let [[server-keys good-client-keys] (gen-key-pair)]
    (with-open [server (server/start-server :tls? true
                                            :tls-keys-str server-keys)]
      (with-open [sock (Socket.)]
        (let [addr (InetSocketAddress. "127.0.0.1" ^int (:port server))]
          (.connect sock addr 1000)))
      (with-open [transport (tls-connect {:tls-keys-str good-client-keys
                                          :host         "127.0.0.1"
                                          :port         (:port server)
                                          :transport-fn transport/bencode})]
        (let [client (nrepl/client transport 30000)]
          (is (= 2 (eval-value1 client '(+ 1 1)))))))))

(#?(:clj deftest :clje comment) happy-case
  (let [[server-keys client-keys] (gen-key-pair)]
    (with-open [server (server/start-server :tls? true :tls-keys-str server-keys)
                transport (tls-connect {:tls-keys-str client-keys
                                        :host         "127.0.0.1"
                                        :port         (:port server)
                                        :transport-fn transport/bencode})]
      (let [client (nrepl/client transport 30000)]
        (is (= 2 (eval-value1 client '(+ 1 1))))))))

(#?(:clj deftest :clje comment) accept-sets-tcp-nodelay
  (let [[server-keys client-keys] (gen-key-pair)]
    (with-open [server (tls/server-socket (tls/ssl-context-or-throw server-keys nil) "127.0.0.1" 0)]
      (let [client (future
                     (doto (tls/socket (tls/ssl-context-or-throw client-keys nil) "127.0.0.1" (.getLocalPort server) 1000)
                       (.startHandshake)))]
        (with-open [^Socket accepted (socket/accept server)
                    ^Socket _client @client]
          (is (.getTcpNoDelay accepted)))))))

(#?(:clj deftest :clje comment) regular-connection-times-out
  (let [[server-keys _] (gen-key-pair)
        exception (promise)]
    (with-redefs [tls/*handshake-timeout-ms* 300]
      (with-open [server (server/start-server :tls? true
                                              :tls-keys-str server-keys
                                              :consume-exception (partial deliver exception))
                  ^AutoCloseable _ (nrepl/connect :port (:port server))]
        (is (tls-socket-exception? exception))))))

(#?(:clj deftest :clje comment) regular-connection+eval-fails
  (let [[server-keys _] (gen-key-pair)
        exception (promise)]
    (with-open [server (server/start-server :tls? true
                                            :tls-keys-str server-keys
                                            :consume-exception (partial deliver exception))
                ^AutoCloseable transport (nrepl/connect :port (:port server))]
      (let [client (nrepl/client transport 30000)]
        (is (thrown? Exception (eval-value1 client '(+ 1 1))))
        (is (tls-socket-exception? exception))))))

(#?(:clj deftest :clje comment) server-keys-then-good
  (let [[server-keys good-client-keys] (gen-key-pair)
        exception (promise)]
    (with-open [server (server/start-server :tls? true
                                            :tls-keys-str server-keys
                                            :consume-exception (partial deliver exception))]
      (with-open [transport (tls-connect {:tls-keys-str server-keys
                                          :host         "127.0.0.1"
                                          :port         (:port server)
                                          :transport-fn (fn [& args]
                                                          ;; there appears to be
                                                          ;; a (race?) condition
                                                          ;; that the
                                                          ;; transport-fn could
                                                          ;; also be called.
                                                          (try
                                                            (apply transport/bencode args)
                                                            (catch Exception e
                                                              (is (instance? SSLHandshakeException e)))))})]
        (let [client (nrepl/client transport 30000)]
          (is (thrown? Exception (eval-value1 client '(+ 1 1))))
          (is (tls-socket-exception? exception))))
      (with-open [transport (tls-connect {:tls-keys-str good-client-keys
                                          :host         "127.0.0.1"
                                          :port         (:port server)
                                          :transport-fn transport/bencode})]
        (let [client (nrepl/client transport 30000)]
          (is (= 2 (eval-value1 client '(+ 1 1)))))))))

(#?(:clj deftest :clje comment) tls-client-proxy-test
  (let [[server-keys client-keys] (gen-key-pair)]
    (with-redefs [tls-client-proxy/atomic-println (fn [& _] nil)]
      (with-open [server (server/start-server :tls? true :tls-keys-str server-keys)]
        (let [state (tls-client-proxy/start-tls-proxy {:remote-host  "127.0.0.1"
                                                       :remote-port  (:port server)
                                                       :tls-keys-str client-keys
                                                       :port-file    nil
                                                       :block?       false})]
          (try
            (let [proxy-port (deref (:port-promise @state) 3000 nil)]
              (when (is (> proxy-port 0))
                (with-open [^AutoCloseable transport (nrepl/connect :port proxy-port)]
                  (let [client (nrepl/client transport 30000)]
                    (is (= 2 (eval-value1 client '(+ 1 1))))))))
            (finally
              (tls-client-proxy/stop! state))))))))

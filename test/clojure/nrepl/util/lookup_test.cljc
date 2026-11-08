(ns nrepl.util.lookup-test
  (:require [clojure.test :refer :all]
            [matcher-combinators.matchers :as matchers]
            [nrepl.bencode :as bencode]
            [nrepl.test-helpers :refer [is+]]
            [nrepl.util.lookup :as l :refer [lookup]]
            #?(:clje [nrepl.beam.bytes :as bytes]))
  #?(:clj (:import (java.io ByteArrayOutputStream))))

(deftest lookup-test
  (testing "special sym lookup"
    (is (not-empty (lookup 'clojure.core 'if))))

  (testing "fully qualified sym lookup"
    (is (not-empty (lookup 'nrepl.util.lookup 'clojure.core/map))))

  (testing "aliased sym lookup"
    (is (not-empty (lookup 'nrepl.util.lookup 'clojure.string/upper-case))))

  (testing "non-qualified lookup"
    (is (not-empty (lookup 'clojure.core 'map)))

    (is+ {:ns "clojure.core"
          :name "map"
          :arglists "([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])"
          :arglists-str "([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])"}
         (lookup 'nrepl.util.lookup 'map))

    (is+ {:ns "clojure.core"
          :name "map"
          :arglists "([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])"
          :arglists-str "([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])"}
         (lookup 'clojure.core 'map)))

  (testing "macro lookup"
    (is+ {:ns "clojure.core"
          :name "future"
          :macro "true"}
         (lookup 'clojure.core 'future)))

  (testing "special form lookup"
    (is+ {:ns "clojure.core"
          :name "let"
          :special-form "true"}
         (lookup 'clojure.core 'let)))

  (testing "Java sym lookup"
    (is+ nil (lookup 'clojure.core 'String)))

  (testing "namespace lookup"
    (is+ {:ns "clojure.string"
          :name "clojure.string"
          :doc #"Clojure String utilities"
          :file #?(:clj #"clojure/string\.clj$" :clje #"clojure/string\.clje$")}
         (lookup 'nrepl.util.lookup-test 'clojure.string))

    (testing "by its alias"
      (is+ {:name "nrepl.bencode"
            :file #"nrepl/bencode\.cljc$"}
           (lookup 'nrepl.util.lookup-test 'bencode)))

    (testing "from a .cljc file"
      (is+ {:name "matcher-combinators.matchers"
            ;; On the BEAM this namespace is the shim in test/clje.
            :file #?(:clj #"matcher_combinators/matchers\.cljc$" :clje #"matcher_combinators/matchers\.clje$")}
           (lookup 'nrepl.util.lookup-test 'matcher-combinators.matchers)))

    (testing "without a file"
      (create-ns 'nrepl.util.lookup-test.no-file)
      (is+ {:name "nrepl.util.lookup-test.no-file"
            :file matchers/absent}
           (lookup 'nrepl.util.lookup-test 'nrepl.util.lookup-test.no-file))))

  (testing "a var before a namespace's alias"
    (is+ {:ns "clojure.core"
          :name "str"}
         (binding [*ns* (create-ns 'nrepl.util.lookup-test.str-alias)]
           (refer-clojure)
           (alias 'str 'clojure.string)
           (lookup 'nrepl.util.lookup-test.str-alias 'str)))))

(defn- bencode-str
  "Bencode a thing and write it into a string."
  [thing]
  (let [out #?(:clj (ByteArrayOutputStream.) :clje (bytes/binary-output))]
    (try
      (bencode/write-bencode out thing)
      #?(:clj (.toString out) :clje (bytes/output->binary out))
      (catch #?(:clj IllegalArgumentException :clje clojerl.BadArgumentError) ex
        (throw (ex-info #?(:clj (.getMessage ex) :clje (ex-message ex)) {:thing thing}))))))

(#?(:clj defn- :clje comment) lookup-public-vars
  "Look up every public var in all namespaces in the classpath and return the result as a set."
  []
  (transduce
   (comp
    (mapcat ns-publics)
    (map (comp meta val))
    (map #(lookup (.getName ^clojure.lang.Namespace (:ns %)) (:name %))))
   conj
   #{}
   (all-ns)))

;; Some clojerl vars have no metadata, and clojerl keys ns-publics by string.
#?(:clje
   (defn- lookup-public-vars
     "Look up every public var in all namespaces and return the result as a set."
     []
     (set (for [ns (apply list (all-ns))
                [sym _] (apply list (ns-publics ns))]
            (lookup (ns-name ns) (symbol sym))))))

(deftest bencode-test
  (doseq [m (lookup-public-vars)]
    (is ((comp string? not-empty) (bencode-str m)))))

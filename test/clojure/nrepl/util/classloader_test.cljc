(ns nrepl.util.classloader-test
  (:require [clojure.test :refer :all]
            #?(:clj [nrepl.util.classloader :as classloader]))
  #?(:clj (:import clojure.lang.DynamicClassLoader)))

;; The BEAM has no classloaders, so these tests run only on the JVM.

(#?(:clj def :clje comment) top-cl (->> (.getContextClassLoader (Thread/currentThread))
                                        (iterate #(.getParent ^ClassLoader %))
                                        (take-while identity)
                                        last))

(#?(:clj deftest :clje comment) find-topmost-dcl-test
  (let [dcl1 (DynamicClassLoader. top-cl)
        dcl2 (DynamicClassLoader. dcl1)]
    (is (= nil (classloader/find-topmost-dcl top-cl)))
    (is (= dcl1 (classloader/find-topmost-dcl dcl1)))
    (is (= dcl1 (classloader/find-topmost-dcl dcl2)))))

(#?(:clj deftest :clje comment) dynamic-classloader-test
  (let [dcl1 (DynamicClassLoader. top-cl)
        dcl2 (DynamicClassLoader. dcl1)]
    (is (instance? DynamicClassLoader (classloader/dynamic-classloader top-cl)))
    (is (= dcl1 (classloader/dynamic-classloader dcl1)))
    (is (= dcl1 (classloader/dynamic-classloader dcl2)))))

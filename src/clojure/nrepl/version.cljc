(ns nrepl.version
  "nREPL version information, read from the Maven-generated POM properties
  at build time."
  {:author "Colin Jones"
   :added  "0.5"}
  #?(:clj (:require [clojure.java.io :as io]))
  #?(:clj (:import java.util.Properties)))

(def ^:private group "nrepl")
(def ^:private artifact "nrepl")
(def ^:private properties-filename
  (str "META-INF/maven/" group "/" artifact "/pom.properties"))

(#?(:clj defn- :clje comment) map-from-property-filepath [file]
  (try
    (with-open [rdr (io/reader (io/resource file))]
      (let [props (Properties.)]
        (.load props rdr)
        (into {} props)))
    (catch Exception _e nil)))

#?(:clje
   (defn- app-vsn
     "Return the vsn of the nrepl application, or nil if it is not on the
  code path. The BEAM build reads it at compile time."
     []
     (application/load :nrepl)
     (let [r (application/get_key :nrepl :vsn)]
       (when (erlang/is_tuple r)
         (erlang/list_to_binary (erlang/element 2 r))))))

(defn- get-version
  "Attempts to get the project version from system properties, or a properties
  file based on the group and artifact ids (in jars built by tools.build), or a
  default version passed in.  Falls back to an empty string when no default is
  present."
  ([] (get-version ""))
  ([default-version]
   (or #?(:clj (System/getProperty (str artifact ".version")) :clje (app-vsn))
       #?(:clj (get (map-from-property-filepath properties-filename) "version"))
       default-version)))

(def ^:private version-string
  "Current version of nREPL as a string.
  See also `version`."
  (get-version))

(def version
  "Current version of nREPL.
  Map of :major, :minor, :incremental, :qualifier, and :version-string."
  (assoc (->> version-string
              (re-find #"(\d+)\.(\d+)\.(\d+)-?(.*)")
              rest
              (map #(try #?(:clj (Integer/parseInt %) :clje (erlang/binary_to_integer %))
                         (catch #?(:clj Exception :clje _) _e nil)))
              (zipmap [:major :minor :incremental :qualifier]))
         :version-string version-string))

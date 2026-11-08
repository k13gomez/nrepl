(ns nrepl.util.lookup
  "Symbol info lookup.

  It's meant to provide you with useful data like definition location,
  parameter lists, etc.

  NOTE: The functionality here is experimental and
  the API is subject to changes."
  {:author "Bozhidar Batsov"
   :added "0.8"}
  (:require
   #?(:clj [clojure.java.io :as io])
   [nrepl.misc :as misc]
   #?(:clje [nrepl.beam.compat :as compat :refer [requiring-resolve]])))

(defn special-sym-meta
  [sym]
  ;; clojure.repl/special-doc is private, so we need to work a
  ;; bit to be able to invoke it
  (let [f (requiring-resolve 'clojure.repl/special-doc)]
    (assoc (f sym)
           :ns "clojure.core"
           :file #?(:clj "clojure/core.clj" :clje "clojure/core.clje")
           :special-form "true")))

#?(:clje
   (defn- var-meta
     "Return the metadata of the var `v`, with its :ns and :name. The vars
  that clojerl defines in Erlang, such as `*assert*`, have no metadata."
     [v]
     (let [s (str v)
           var-sym (symbol (erlang/binary_part s 2 (- (erlang/byte_size s) 2)))]
       (merge {:ns (symbol (namespace var-sym)) :name (symbol (name var-sym))}
              (meta v)))))

(defn normal-sym-meta
  [ns sym]
  (some-> (ns-resolve ns sym) #?(:clj meta :clje var-meta)))

(#?(:clj defn- :clje comment) ns-file
  "The classpath-relative path of the file `the-ns` loads from, if any."
  [the-ns]
  (let [base (-> (name (ns-name the-ns)) (.replace \- \_) (.replace \. \/))]
    (first (filter io/resource (map #(str base %) [".clj" ".cljc"])))))

#?(:clje
   (defn- ns-file
     "The path of the file `the-ns` loads from, if any. The BEAM has no
  classpath resources, so this takes the `:file` metadata of its vars."
     [the-ns]
     (some (fn [[_ v]] (:file (meta v))) (ns-interns the-ns))))

(defn ns-sym-meta
  "The metadata of the namespace `sym` names, an alias in `ns` or the
  namespace's own name."
  [ns sym]
  ;; clojerl keys the aliases by string.
  (when-let [the-ns (or (get (ns-aliases ns) #?(:clj sym :clje (str sym))) (find-ns sym))]
    (let [ns-sym (ns-name the-ns)
          file (ns-file the-ns)]
      (cond-> (assoc (meta the-ns) :ns ns-sym :name ns-sym)
        file (assoc :file file)))))

(defn sym-meta
  [ns sym]
  (if (special-symbol? sym)
    (special-sym-meta sym)
    (or (normal-sym-meta ns sym)
        (ns-sym-meta ns sym))))

(defn lookup
  "Lookup the metadata for `sym`, a var, a special form or a namespace.
  If the `sym` is not qualified then it will be resolved in the context
  of `ns`, where it can also be a namespace's alias."
  [ns sym]
  ;; A namespace that clojerl loads from a beam has no refers or aliases
  ;; until they are restored.
  #?(:clje (some-> (find-ns ns) compat/restore-ns-mappings!))
  (some-> (sym-meta ns sym) misc/sanitize-meta))

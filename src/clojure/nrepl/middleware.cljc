(ns nrepl.middleware
  "Middleware descriptor infrastructure.

  Provides `set-descriptor!` for declaring middleware metadata
  (dependencies, handled ops, documentation) and
  `linearize-middleware-stack` for ordering middleware according
  to their `:requires` and `:expects` constraints. Also includes
  the built-in `wrap-describe` middleware."
  {:added "0.4"}
  (:refer-clojure :exclude [comparator #?@(:clje [format])])
  (:require
   [nrepl.misc :as misc]
   [nrepl.transport :as t :refer [safe-handle]]
   [nrepl.version :as version]
   #?@(:clje [[nrepl.beam.compat :refer [descriptor-of format register-descriptor!]]])))

;; Registering dynvars that are used to configure middleware. This lives here
;; and not in nrepl.middleware.session to avoid circular dependencies.

(def ^:private per-session-dynvars
  "Set of dynamic variables that always have to be established for a new session
  to their initial values."
  (atom #{}))

(defn- var-name
  [^#?(:clj clojure.lang.Var :clje clojerl.Var) v]
  #?(:clj (str (.ns v) \/ (.sym v))
     :clje (let [m (meta v)] (str (:ns m) \/ (:name m)))))

(defn #?(:clj set-descriptor! :clje set-descriptor-fn)
  "Sets the given [descriptor] map as the ::descriptor metadata on the
  provided [middleware-var], after assoc'ing in the var's fully-qualified name
  as the descriptor's \"implemented-by\" value. The value of `:session-dynvars`
  should be a set of dynamic variables that the middleware expects to be made
  rebindable for each new session."
  [middleware-var descriptor]
  (let [descriptor (assoc descriptor
                          :implemented-by (-> middleware-var var-name symbol))]
    (swap! per-session-dynvars into (:session-dynvars descriptor))
    #?(:clj (alter-meta! middleware-var assoc ::descriptor descriptor)
       :clje (register-descriptor! middleware-var descriptor))))

#?(:clje
   (defmacro set-descriptor!
     "Sets the given [descriptor] map as the descriptor of the provided
  [middleware-var], like the JVM function. An AOT-compiled namespace does
  not run its top-level calls at load, but it runs `erl-on-load*`, so this
  macro registers the descriptor both now and when the module loads.

  clojerl cannot call its own module from `on_load`. So in this namespace
  the descriptor goes straight into `nrepl.beam.compat`."
     [middleware-var descriptor]
     (if (= 'nrepl.middleware (ns-name *ns*))
       (let [[_ sym] middleware-var]
         `(do (set-descriptor-fn ~middleware-var ~descriptor)
              (erl-on-load*
               (register-descriptor! ~middleware-var
                                     (assoc ~descriptor :implemented-by '~(symbol (str *ns*) (str sym)))))))
       `(do (nrepl.middleware/set-descriptor-fn ~middleware-var ~descriptor)
            (erl-on-load* (nrepl.middleware/set-descriptor-fn ~middleware-var ~descriptor))))))

(defn- safe-version
  [m]
  (into {} (filter (fn [[_ v]] (or (number? v) (string? v))) m)))

(#?(:clj defn- :clje comment) java-version
  []
  {:major misc/java-version
   :version-string (System/getProperty "java.version")})

#?(:clje
   (defn- erlang-version
     "Return the OTP release and the ERTS version of the running VM."
     []
     (let [otp (erlang/list_to_binary (erlang/system_info :otp_release))]
       {:major (erlang/binary_to_integer otp)
        :version-string otp
        :erts-version-string (erlang/list_to_binary (erlang/system_info :version))})))

(defn- describe-reply
  [{:keys [verbose?] :as msg} middleware]
  (let [descriptors (mapv #?(:clj (comp ::descriptor meta) :clje descriptor-of) middleware)
        ops (let [ops (apply merge (map :handles descriptors))]
              (if verbose?
                ops
                (zipmap (keys ops) (repeat {}))))
        aux (reduce
             (fn [aux {:keys [describe-fn]}]
               (cond-> aux
                 describe-fn (merge (describe-fn msg))))
             nil descriptors)]
    (merge
     {:ops ops
      :middleware (mapv str middleware)
      :versions {:nrepl (safe-version version/version)
                 :clojure (safe-version
                           (assoc *clojure-version* :version-string (clojure-version)))
                 #?@(:clj [:java (safe-version (java-version))]
                     :clje [:erlang (safe-version (erlang-version))])}
      :status :done}
     (when aux {:aux aux}))))

(defn wrap-describe
  "Special middleware that accepts the list of all other middleware (except self)
  and handles `describe` op."
  [h middleware]
  (let [middleware+self (conj middleware #'wrap-describe)]
    (fn [msg]
      (safe-handle msg
        "describe" #(describe-reply % middleware+self)
        :else h))))

(set-descriptor! #'wrap-describe
                 {:handles {"describe"
                            {:doc "Produce a machine- and human-readable directory and documentation for the operations supported by an nREPL endpoint."
                             :requires {}
                             :optional {"verbose?" "Include informational detail for each \"op\"eration in the return message."}
                             :returns {"ops" "Map of \"op\"erations supported by this nREPL endpoint"
                                       "middleware" "Current middleware stack, in inside-out order (earliest middleware is last)."
                                       "versions" "Map containing version maps (like *clojure-version*, e.g. major, minor, incremental, and qualifier keys) for values, component names as keys. Common keys include \"nrepl\" and \"clojure\"."
                                       "aux" "Map of auxiliary data contributed by all of the active nREPL middleware via :describe-fn functions in their descriptors."}}}})

(defn- comparator
  [{a-requires :requires a-expects :expects a-handles :handles}
   {b-requires :requires b-expects :expects b-handles :handles}]
  (letfn [(intersect? [set1 set2]
            (let [set1 (set set1)]
              (reduce (fn [_ el]
                        (when (contains? set1 el)
                          (reduced true)))
                      nil set2)))]
    (cond (intersect? a-requires b-handles) -1
          (intersect? a-expects b-handles) 1
          (intersect? b-requires a-handles) 1
          (intersect? b-expects a-handles) -1
          :else 0)))

(defn- extended-descriptors
  "This function serves two purposes:
  - Scan through middlewares' dependencies and add those that are mentioned in
  `:requires` or `:expects`, but not included explicitly. This is done for
  compatibility, but will be removed in the future.
  - Return descriptor for each middleware."
  [middlewares]
  (let [mware-set (set middlewares)]
    (into []
          (comp (mapcat (fn [m]
                          (let [{:keys [expects requires] :as desc} #?(:clj (::descriptor (meta m)) :clje (descriptor-of m))]
                            (when (nil? desc)
                              (misc/log :warning "No nREPL middleware descriptor in metadata of" m
                                        ", see nrepl.middleware/set-descriptor!"))
                            (let [deps (concat expects requires)
                                  missing-deps (for [dep deps
                                                     :when (and (var? dep) (not (mware-set dep)))]
                                                 (do (misc/log :warning (format "Middleware %s is required by %s but is not present in middleware list." dep m))
                                                     dep))]
                              ;; TODO: stop adding missing deps in future versions.
                              (into [m] missing-deps)))))
                (distinct)
                (map (fn [m]
                       (let [desc #?(:clj (::descriptor (meta m)) :clje (descriptor-of m))]
                         (-> desc
                             ;; Conj the middleware Var itself to `:handles` to support direct
                             ;; reference to middlewares dependencies in `:expects`/`:requires`.
                             (update :handles #(into #{m} (keys %)))
                             ;; (update :handles #(into #{m} (keys %)))
                             (assoc :implemented-by m))))))
          middlewares)))

(defn- topologically-sort
  "Topologically sorts the given middleware descriptors according to
  expects/requires rules with the added heuristic that any middlewares that have
  no dependencies will be sorted toward the end."
  [stack]
  (let [stack (vec stack)
        ;; using indexes into the above vector as the vertices in the
        ;; graph algorithm, will translate back into middlewares at
        ;; the end.
        vertices (range (count stack))
        edges (vec (for [i1 vertices
                         i2 (range i1)
                         :let [x (comparator (stack i1) (stack i2))]
                         :when (not= 0 x)]
                     (if (neg? x) [i1 i2] [i2 i1])))
        connected (into #{} cat edges)
        ;; the trivial vertices have no connections, and we pull them
        ;; out here so we can make sure they get put on the end
        trivial-vertices (remove connected vertices)]
    (loop [sorted-vertices []
           remaining-edges edges
           remaining-vertices (filterv connected vertices)]
      (if (seq remaining-vertices)
        (let [non-initials (into #{} (map #(nth % 1)) remaining-edges)
              next-vertex (reduce (fn [_ v]
                                    (when-not (non-initials v)
                                      (reduced v)))
                                  nil remaining-vertices)]
          (if next-vertex
            (recur (conj sorted-vertices next-vertex)
                   (into [] (remove #(or (= next-vertex (nth % 0))
                                         (= next-vertex (nth % 1)))
                                    remaining-edges))
                   (into [] (remove #(= % next-vertex)) remaining-vertices))
            ;; Cycle detected! Have to actually assemble a cycle so we
            ;; can throw a useful error.
            (let [start (first remaining-vertices)
                  step (into {} remaining-edges)
                  cycle (->> (iterate step start)
                             (rest)
                             (take-while (complement #{start}))
                             (cons start))
                  data {:cycle (map stack cycle)}]
              (throw (ex-info
                      "Unable to satisfy nREPL middleware ordering requirements!"
                      data)))))
        (mapv stack (into sorted-vertices trivial-vertices))))))

(defn linearize-middleware-stack
  "Given a list of middleware vars, sort them topologically, so that least
  dependent middleware come FIRST and ones dependent on come LAST. The returned
  order lends itself to being composed as-is into the final handler."
  [middleware-vars]
  (->> (extended-descriptors middleware-vars)
       topologically-sort
       (map :implemented-by)))

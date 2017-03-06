(ns workflo.query-engine.data-layer.util
  (:refer-clojure :exclude [sort])
  (:require [clojure.string :as str]
            [workflo.macros.entity.schema :as es]
            [workflo.query-engine.util :as util]))

;;;; Data fetching

(defn has-entity-attrs-rule [attrs]
  (into '[(has-entity-attrs? ?e)]
        (comp (remove #{:db/id})
              (map (fn [attr] ['?e attr])))
        attrs))

;;;; Result processing, filtering, sorting etc.

(def ^:private sort-params
  #{:sort/attr :sort/order})

(def ^:private pagination-params
  #{:page/after-id :page/count})

(def ^:private reserved-params
  (clojure.set/union sort-params pagination-params))

(defn- filter-param?
  [[k v]]
  (not (some #{k} reserved-params)))

;;; Filtering

(defn reserved-param?
  [[k v]]
  (some #{k} reserved-params))

(defn filter-entity
  [data entity params]
  (if-let [filter-params (seq (filter filter-param? params))]
    (let [refs (util/memoized-entity-refs (:name entity))]
      (when (every? (fn [[k v]]
                      (if (get refs k)
                        (if (:many? (get refs k))
                          (or (some #{v} (get data k))
                              (some #{v} (map :db/id (get data k))))
                          (or (= v (get data k))
                              (= v (:db/id (get data k)))))
                        (= (get data k) v)))
                    filter-params)
        data))
    data))

;;; Rule generation from parameters

(defn- backref-attr?
  "Returns true if `attr` is a backref symbol or keyword, i.e., its
   name segment begins with an underscore."
  [attr]
  (let [ns (namespace attr)
        nm (name attr)]
    (str/starts-with? nm "_")))

(defn- forward-attr
  "Returns a keyword that represents the corresponding forward
   attribute to the backref attribute `attr`. Essentially strips
   off the underscore from the name segment."
  [attr]
  (let [ns (namespace attr)
        nm (name attr)]
    (if (str/starts-with? nm "_")
      (keyword ns (subs nm 1))
      (keyword ns nm))))

(defn- attr-clause
  "Creates a Datalog clause for an attribute. Takes in the name of
   the previous (reference) variable, the attribute name and the
   target var or value. If `attr` is a backref attribute, it will
   convert it to a forward attribute and reverse `prev-var` and
   `var-or-value` in the generated clause."
  [prev-var attr var-or-value]
  (cond
    (= :db/id attr)      [`(~'= ~prev-var ~var-or-value)]
    (backref-attr? attr) [var-or-value (forward-attr attr) prev-var]
    :else                [prev-var attr var-or-value]))

(defn validate-param
  [param]
  (when (vector? param)
    (loop [attr (first param) path (rest param)]
      (when-not (empty? path)
        (if (= :db/id attr)
          (throw (ex-info (str ":db/id must be the last item in a parameter path: " param)
                          {:parameter-path param}))
          (recur (first path) (rest path)))))))

(defn- var-at-index
  "Takes an index and returns a Datalog variable symbol `?var-<index>`."
  [index]
  (symbol (str "?var-" index)))

(defn- path-vars
  "Takes a path of attributes and returns as many Datalog variables as
   there are attributes, with the first one being called `?e`. E.g. the
   path `[:foo :bar :baz]` would result in `[?e ?var-1 ?var-2]`."
  [path]
  (into ['?e] (map var-at-index) (map inc (range (dec (count path))))))

(defn- path-clauses
  "Takes a path of attributes and a value. Returns attribute clauses
   that lead from the Datalog variable ?e to the value via the
   attributes.

   Example: The path `[:foo :bar :baz]` and the value `1234` would
   result in the following clauses:
   ```
   [?e :foo ?var-1]
   [?var-1 :bar ?var-2]
   [?var-2 :baz 1234]
   ```"
  [path value]
  (let [vars (path-vars path)]
    (map attr-clause vars path (conj (subvec vars 1) value))))

(defn matches-param-rule
  "Takes a parameter (can be an attribute path), a value and
   a suffix for the Datalog rule to generate. Generates a
   `(matches-param-<suffix>? ?e)` rule that matches all `?e`
   that leads to `value` via the attribute(s) in `param`."
  [[param value] suffix]
  (validate-param param)
  (let [rule-name (symbol (str "matches-param-" suffix "?"))
        path      (cond-> param (not (vector? param)) vector)]
    (into [`(~rule-name ~'?e)] (path-clauses path value))))

(defn matches-params-rules
  "Takes a parameter map (e.g. from a query) and generates a number
   of Datalog rules to match entities against these parameters. The
   entry point to these rules is the `(matches-params? ?e)` rule."
  [params]
  (let [param-rules (transduce
                     (comp (remove reserved-param?)
                           (map-indexed vector)
                           (map (fn [[index param]]
                                  (matches-param-rule param (inc index)))))
                     conj [] params)]
    `[[~'(matches-params? ?e)
       ~'[?e]
       ~@(map first param-rules)]
      ~@param-rules]))

(def matches-params-rules*
  "A memoized version of `matches-params-rules`."
  (memoize matches-params-rules))

;;; Sorting

(defn sort
  [params entities]
  (if-let [sort-attr (:sort/attr params)]
    (vec (cond-> (sort-by sort-attr entities)
           (= :sort/descending (:sort/order params)) reverse))
    entities))

;;; Pagination

(defn paginate
  [params entities]
  (if (some pagination-params (keys params))
    (let [after-id (:page/after-id params)
          count (or (:page/count params) (count entities))]
      (if after-id
        (->> entities
             (drop-while #(not= (:db/id %) after-id))
             (rest)
             (take count)
             (vec))
        (->> entities
             (take count)
             (vec))))
    entities))

;;; Selecting attributes

(def select-attrs select-keys)

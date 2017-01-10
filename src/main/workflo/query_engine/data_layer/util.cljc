(ns workflo.query-engine.data-layer.util
  (:refer-clojure :exclude [sort])
  (:require [workflo.macros.entity.schema :as es]
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

(defn- reserved-param?
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

(defn matches-param-rule
  "Takes a param/value tuple and a rule name suffix. Returns a
   `(matches-param-<suffix>? ?e)` rule that binds ?e to all entity
   IDs that match the param/value pair.

   Param may be either a single attribute name (e.g. `:db/id`) or
   a vector of attributes (a path) (e.g. `[:account/users :db/id]`).
   The latter generates a chain of Datalog clauses that traverse
   the attribute path and match the final attribute against the
   parameter's value."
  [[param value] suffix]
  (let [rule-name      (symbol (str "matches-param-" suffix "?"))
        rule-signature `(~rule-name ~'?e)]
    (into [rule-signature]
          (cond
            (vector? param)  (loop [exprs     []
                                    attr      (first param)
                                    path      (rest param)
                                    prev-var  '?e
                                    var-index 1]
                               (if (empty? path)
                                 (conj exprs (if (= :db/id attr)
                                               [`(~'= ~prev-var ~value)]
                                               [prev-var attr value]))
                                 (let [var (symbol (name (str "?var-" var-index)))]
                                   (recur (conj exprs [prev-var attr var])
                                          (first path)
                                          (rest path)
                                          var
                                          (inc var-index)))))
            (= :db/id param) [[`(~'= ~'?e ~value)]]
            :else            [['?e param value]]))))

(defn matches-params-rules
  "Takes a parameter map (e.g. from a query) and generates a number
   of Datalog rules to match entities against these parameters.

   The entry point to these rules is the `(matches-params? ?e)` rule."
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

(defn select-attrs
  [data attrs]
  (select-keys data attrs))

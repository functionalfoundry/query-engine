(ns workflo.query-engine.data-layer.util
  (:refer-clojure :exclude [sort]))

(defonce ^:private sort-params
  #{:sort/attr :sort/order})

(defonce ^:private paginate-params
  #{:paginate/after-index :paginate/count})

(defonce ^:private non-filter-params
  (clojure.set/union sort-params paginate-params))

(defn- filter-param?
  [[k v]]
  (not (some #{k} non-filter-params)))

(defn filter-entity
  [data params]
  (let [filter-params (filter filter-param? params)]
    (when (and data filter-params
               (every? (fn [[k v]]
                         (= (get data k) v))
                       filter-params))
      data)))

(defn sort
  [params entities]
  (if-let [sort-attr (:sort/attr params)]
    (vec (cond-> (sort-by sort-attr entities)
           (= :sort/descending (:sort/order params)) reverse))
    (set entities)))

(defn paginate
  [params entities]
  (if (some paginate-params (keys params))
    (let [after-index (or (:paginate/after-index params) -1)
          count (or (:paginate/count params) (count entities))]
      (into [] (comp (drop (inc after-index))
                     (take count))
            entities))
    entities))

(defn select-attrs
  [data attrs]
  (select-keys data attrs))

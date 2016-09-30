(ns workflo.query-engine.data-layer.util
  (:refer-clojure :exclude [sort]))

(defonce ^:private non-filter-params
  #{:sort/attr :sort/order})

(defn filter-entity
  [data params]
  (let [filter-params (remove (comp non-filter-params first) params)]
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

(defn select-attrs
  [data attrs]
  (select-keys data attrs))

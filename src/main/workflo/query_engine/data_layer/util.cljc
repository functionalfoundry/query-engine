(ns workflo.query-engine.data-layer.util
  (:refer-clojure :exclude [sort]))

(def ^:private sort-params
  #{:sort/attr :sort/order})

(def ^:private pagination-params
  #{:page/after-id :page/count})

(def ^:private non-filter-params
  (clojure.set/union sort-params pagination-params))

(defn- filter-param?
  [[k v]]
  (not (some #{k} non-filter-params)))

(defn filter-entity
  [data params]
  (let [filter-params (seq (filter filter-param? params))]
    (when (every? (fn [[k v]]
                    (= (get data k) v))
                  filter-params)
      data)))

(defn sort
  [params entities]
  (if-let [sort-attr (:sort/attr params)]
    (vec (cond-> (sort-by sort-attr entities)
           (= :sort/descending (:sort/order params)) reverse))
    (set entities)))

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

(defn select-attrs
  [data attrs]
  (select-keys data attrs))

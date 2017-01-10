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

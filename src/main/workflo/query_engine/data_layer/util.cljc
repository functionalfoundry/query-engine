(ns workflo.query-engine.data-layer.util)

(defn filter-entity
  [data params]
  (when (and data
             params
             (every? (fn [[k v]]
                       (= (get data k) v))
                     params))
    data))

(defn select-attrs
  [data attrs]
  (select-keys data attrs))

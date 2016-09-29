(ns workflo.query-engine.cache.atom
  (:require [workflo.query-engine.cache :as c :refer [Cache]]))

(defn atom-cache []
  (let [data (atom {})]
    (reify Cache
      (set-one [_ k v]
        (swap! data assoc k v)
        v)
      (set-many [_ kvs]
        (swap! data merge kvs)
        (vals kvs))
      (get-one [_ k]
        (get @data k))
      (get-many [this ks fetcher]
        (let [res (select-keys @data ks)
              hits (vals res)
              misses (clojure.set/difference (set ks)
                                             (set (keys res)))]
          (if (and (not (empty? misses))
                   (fn? fetcher))
            (let [fetched (c/set-many this (fetcher misses))]
              (concat hits fetched))
            hits))))))

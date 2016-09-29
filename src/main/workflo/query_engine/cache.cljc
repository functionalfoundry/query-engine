(ns workflo.query-engine.cache)

(defprotocol Cache
  (set-one [this k v])
  (set-many [this kvs])
  (get-one [this k])
  (get-many [this ks fetcher]))

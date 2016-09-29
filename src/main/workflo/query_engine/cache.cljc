(ns workflo.query-engine.cache)

(defprotocol Cache
  (set-one [this k v])
  (set-many [this kvs])
  (get-one [this k fetcher])
  (get-many [this ks fetcher]))

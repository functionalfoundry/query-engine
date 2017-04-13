(ns workflo.query-engine.core
  (:require [workflo.query-engine.query :as q]))

;;;; Query engine

(defn query
  "Processes an Om Next query against a given data layer. The
   environment is passed on to the data layer as-is. Special
   keys in the environment are also used by the query processing
   directly (in addition to being passed on to the data layer):

   `:id-attr` (optional) specifies the attribute used for denoting
   entity IDs; can be used to improve the performance of queries
   with `{<id attr> <id>}` in params."
  ([query' data-layer env]
   (query query' data-layer env {}))
  ([query' data-layer env opts]
   (q/process-query query' data-layer env opts)))

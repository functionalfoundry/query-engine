(ns workflo.query-engine.core
  (:require [workflo.query-engine.query :as q]))

;;;; Query engine

(defn query
  "Processes an Om Next query against a given data layer. The
   environment is passed on to the data layer as-is."
  ([query' data-layer env]
   (query query' data-layer env {}))
  ([query' data-layer env opts]
   (q/process query' data-layer env opts)))

(ns workflo.query-engine.data-layer)

(defprotocol DataLayer
  (fetch-one [this env entity id params attrs])
  (fetch-many [this env entity ids params attrs])
  (fetch-all [this env entity params attrs]))

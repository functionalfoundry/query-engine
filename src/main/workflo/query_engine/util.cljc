(ns workflo.query-engine.util
  (:require [inflections.core :as inflections]
            [workflo.macros.entity :as e]))


(def singular
  "Memoized version of `inflections.core/singular`."
  (memoize inflections/singular))


(defn qualified-name
  [sym-or-kw]
  (let [ns (namespace sym-or-kw)
        nm (name sym-or-kw)]
    (cond->> nm
      ns (str ns "/"))))


(def memoized-entity-refs
  (memoize e/entity-refs))


(defn entity-from-query-key
  "Takes a query key (e.g. :users or :user) and resolves it
   it into the corresponding entity definition."
  [k]
  (or (try (e/resolve-entity (symbol (qualified-name k)))
           (catch #?(:cljs js/Error :clj Exception) e))
      (try (e/resolve-entity (symbol (-> (qualified-name k)
                                         (singular))))
           (catch #?(:cljs js/Error :clj Exception) e))))


(def memoized-entity-from-query-key
  (memoize entity-from-query-key))

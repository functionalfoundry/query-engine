(ns workflo.query-engine.data-layer.entitydb
  (:require [workflo.entitydb.core :as entitydb]
            [workflo.macros.entity :as entities]
            [workflo.macros.query.util :as query.util]
            [workflo.query-engine.cache :as c]
            [workflo.query-engine.data-layer :refer [DataLayer]]
            [workflo.query-engine.data-layer.util :as util]))


(defn lookup-entity* [db entity-name id]
  (when (string? id)
    (entitydb/get-by-id db (keyword entity-name) id)))


(def lookup-entity (memoize lookup-entity*))


(defn resolve-refs [db entity-name refs]
  (into #{} (comp (map :workflo/id)
                  (map (partial lookup-entity db entity-name)))
        refs))


(defn backref-attr->forward-attr [k]
  (keyword (namespace k) (subs (name k) 1)))


(defn resolve-backrefs* [db backref forward-attr refs]
  (into #{} (filter (fn [source-data]
                      (when-let [ref-or-refs (get source-data forward-attr)]
                        (if (find ref-or-refs :workflo/id)
                          (some #{(get ref-or-refs :workflo/id)} (map :workflo/id refs))
                          (some (into #{} (map :workflo/id ref-or-refs)) (map :workflo/id refs))))))
        (vals (entitydb/entity-map db (keyword (:entity backref))))))


(def resolve-backrefs (memoize resolve-backrefs*))
(def entity-refs (memoize entities/entity-refs))
(def entity-backrefs (memoize entities/entity-backrefs))


(defn resolve-path [db entity data-set path]
  (loop [data-entity (:name entity)
         data-set    data-set
         path        path]
    (if-not (seq path)
      data-set
      (let [attr            (first path)
            forward-attr    (cond-> attr
                              (query.util/backref-attr? attr)
                              (backref-attr->forward-attr))
            entity-refs     (when-not (query.util/backref-attr? attr)
                              (entity-refs data-entity))
            entity-backrefs (when (query.util/backref-attr? attr)
                              (entity-backrefs data-entity))
            attr-ref        (get entity-refs attr)
            attr-backref    (get entity-backrefs forward-attr)]
        (cond
          attr-ref     (let [refs (if (:many? attr-ref)
                                    (into #{} (comp (mapcat #(get % attr))
                                                    (keep identity))
                                          data-set)
                                    (into #{} (keep #(get % attr)) data-set))]
                         (recur (:entity attr-ref)
                                (resolve-refs db (:entity attr-ref) refs)
                                (rest path)))
          attr-backref (recur (:entity attr-backref)
                              (resolve-backrefs db attr-backref forward-attr data-set)
                              (rest path))
          :else        (recur data-entity
                              (into #{} (map #(get % attr)) data-set)
                              (rest path)))))))


(defn matches-param? [db entity data param value]
  (let [path       (cond-> param
                     (not (sequential? param)) vector)
        result-set (resolve-path db entity [data] path)]
    (and (seq result-set)
         (some (fn [result-value]
                 (or (= result-value value)
                     (and (set? value)
                          (some #{result-value} value))))
               result-set))))


(defn matches-params? [db entity data params]
  (every? (fn [[param value]]
            (matches-param? db entity data param value))
          params))


(defn fetch-one-by-id
  [{:keys [db] :as env} entity id params]
  (when-let [data (lookup-entity db (:name entity) id)]
    (when (matches-params? db entity data params)
      (with-meta data {:entity entity}))))


(defn fetch-entity
  [env entity id params]
  (fetch-one-by-id env entity id params))


(defn fetch-entities
  ([{:keys [db] :as env} entity params]
   (let [ids (keys (entitydb/entity-map db (keyword (:name entity))))]
     (fetch-entities env entity ids params)))
  ([env entity ids params]
   (keep #(fetch-one-by-id env entity % params) ids)))


(defn data-layer []
  (reify DataLayer
    (fetch-one [_ env entity id params attrs]
      (some-> (fetch-entity env entity id (remove util/reserved-param? params))
              (util/select-attrs attrs)))
    (fetch-many [_ env entity ids params attrs]
      (some->> (fetch-entities env entity ids (remove util/reserved-param? params))
               (into #{} (map #(util/select-attrs % attrs)))
               (util/sort params)
               (util/paginate params)))
    (fetch-all [_ env entity params attrs]
      (some->> (fetch-entities env entity (remove util/reserved-param? params))
               (into #{} (map #(util/select-attrs % attrs)))
               (util/sort params)
               (util/paginate params)))))

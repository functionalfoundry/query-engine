(ns workflo.query-engine.data-layer.datascript-test
  (:require [clojure.test :refer [are deftest is]]
            [datascript.core :as d]
            [workflo.macros.entity :as e]
            [workflo.macros.entity.datascript :as ed]
            [workflo.query-engine.cache.atom :refer [atom-cache]]
            [workflo.query-engine.data-layer :as data-layer]
            [workflo.query-engine.data-layer.datascript :as dsl]
            [workflo.query-engine.test-entities]
            [workflo.query-engine.test-data :as test-data]))

(def test-schema
  (->> (e/registered-entities)
       (vals)
       (map ed/entity-schema)
       (apply merge)))

;;;; Fetch individual entities

(defn test-fetch-one
  [{:keys [cache]}]
  (println (if cache
             "Test fetching one with cache"
             "Test fetching one without cache"))
  (let [conn (d/create-conn test-schema)
        layer (dsl/data-layer)
        tx (d/transact! conn test-data/combined)
        resolve-id (fn [tempid] ((:tempids tx) tempid))]
    (are [args result]
        (= result
           (let [[entity id params attrs] args]
             (data-layer/fetch-one layer
                                   {:db (d/db conn)
                                    :cache cache}
                                   (e/resolve-entity entity)
                                   id params attrs)))

      ;; Fetch account once
      ['account (resolve-id -1) {} [:account/name]]
      {:account/name "Company A"}

      ;; Fetch account twice to ensure it still works
      ;; (the cache might break it)
      ['account (resolve-id -1) {} [:account/name]]
      {:account/name "Company A"}

      ;; Fetch the same account again but this time with
      ;; more attributes
      ['account (resolve-id -1) {}
       [:db/id :account/name :account/users]]
      {:db/id (resolve-id -1)
       :account/name "Company A"
       :account/users [{:db/id (resolve-id -10)}
                       {:db/id (resolve-id -11)}]}

      ;; Fetch a different entity
      ['component-library (resolve-id -100) {}
       [:component-library/name
        :component-library/account
        :component-library/creator
        :component-library/components]]
      {:component-library/name "Shop Components"
       :component-library/account {:db/id (resolve-id -1)}
       :component-library/creator {:db/id (resolve-id -10)}
       :component-library/components [{:db/id (resolve-id -1000)}
                                      {:db/id (resolve-id -1001)}]})))

(deftest fetch-one-without-cache
  (test-fetch-one {:cache nil}))

(deftest fetch-one-with-cache
  (test-fetch-one {:cache (atom-cache)}))

;;;; Fetch a collection of entities

(defn test-fetch-many
  [{:keys [cache]}]
  (println (if cache
             "Test fetching many with cache"
             "Test fetching many without cache"))
  (let [conn (d/create-conn test-schema)
        layer (dsl/data-layer)
        tx (d/transact! conn test-data/combined)
        resolve-id (fn [tempid] ((:tempids tx) tempid))]
    (are [args result]
        (= result
           (let [[entity ids params attrs] args]
             (data-layer/fetch-many layer
                                    {:db (d/db conn)
                                     :cache cache}
                                    (e/resolve-entity entity)
                                    ids params attrs)))

      ;; Fetch accounts once
      ['account [(resolve-id -1)] {} [:account/name]]
      #{{:account/name "Company A"}}

      ;; Fetch multiple accounts
      ['account [(resolve-id -1) (resolve-id -2)] {} [:account/name]]
      #{{:account/name "Company A"}
        {:account/name "Company B"}}

      ;; Fetch multiple accounts with more attributes
      ['account [(resolve-id -1) (resolve-id -2)] {}
       [:db/id :account/name :account/users]]
      #{{:db/id (resolve-id -1)
         :account/name "Company A"
         :account/users [{:db/id (resolve-id -10)}
                         {:db/id (resolve-id -11)}]}
        {:db/id (resolve-id -2)
         :account/name "Company B"
         :account/users [{:db/id (resolve-id -12)}]}}

      ;; Fetch instances of a different entity
      ['component-library [(resolve-id -100) (resolve-id -101)] {}
       [:component-library/name
        :component-library/account
        :component-library/creator
        :component-library/components]]
      #{{:component-library/name "Shop Components"
         :component-library/account {:db/id (resolve-id -1)}
         :component-library/creator {:db/id (resolve-id -10)}
         :component-library/components [{:db/id (resolve-id -1000)}
                                        {:db/id (resolve-id -1001)}]}
        {:component-library/name "Social Network Components"
         :component-library/account {:db/id (resolve-id -1)}
         :component-library/creator {:db/id (resolve-id -10)}
         :component-library/components [{:db/id (resolve-id -1002)}
                                        {:db/id (resolve-id -1003)}]}})))

(deftest fetch-many-without-cache
  (test-fetch-many {:cache nil}))

(deftest fetch-many-with-cache
  (test-fetch-many {:cache (atom-cache)}))

;;;; Fetch all instances of an entity

(defn test-fetch-all
  [{:keys [cache]}]
  (println (if cache
             "Test fetching all with cache"
             "Test fetching all without cache"))
  (let [conn (d/create-conn test-schema)
        layer (dsl/data-layer)
        tx (d/transact! conn test-data/combined)
        resolve-id (fn [tempid] ((:tempids tx) tempid))]
    (are [args result]
        (= result
           (let [[entity params attrs] args]
             (data-layer/fetch-all layer
                                   {:db (d/db conn)
                                    :cache cache}
                                   (e/resolve-entity entity)
                                   params attrs)))

      ;; Fetch all accounts once
      ['account {} [:account/name]]
      #{{:account/name "Company A"}
        {:account/name "Company B"}}

      ;; Fetch all accounts again
      ['account {} [:account/name]]
      #{{:account/name "Company A"}
        {:account/name "Company B"}}

      ;; Fetch all accounts with more attributes
      ['account {} [:db/id :account/name :account/users]]
      #{{:db/id (resolve-id -1)
         :account/name "Company A"
         :account/users [{:db/id (resolve-id -10)}
                         {:db/id (resolve-id -11)}]}
        {:db/id (resolve-id -2)
         :account/name "Company B"
         :account/users [{:db/id (resolve-id -12)}]}}

      ;; Fetch all instances of a different entity
      ['component-library {} [:component-library/name
                              :component-library/account
                              :component-library/creator
                              :component-library/components]]
      #{{:component-library/name "Shop Components"
         :component-library/account {:db/id (resolve-id -1)}
         :component-library/creator {:db/id (resolve-id -10)}
         :component-library/components [{:db/id (resolve-id -1000)}
                                        {:db/id (resolve-id -1001)}]}
        {:component-library/name "Social Network Components"
         :component-library/account {:db/id (resolve-id -1)}
         :component-library/creator {:db/id (resolve-id -10)}
         :component-library/components [{:db/id (resolve-id -1002)}
                                        {:db/id (resolve-id -1003)}]}
        {:component-library/name "Event Site Components"
         :component-library/account {:db/id (resolve-id -2)}
         :component-library/creator {:db/id (resolve-id -11)}
         :component-library/components [{:db/id (resolve-id -1004)}]}})))

(deftest fetch-all-without-cache
  (test-fetch-all {:cache nil}))

(deftest fetch-all-with-cache
  (test-fetch-all {:cache (atom-cache)}))

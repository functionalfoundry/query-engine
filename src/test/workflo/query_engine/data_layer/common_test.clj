(ns workflo.query-engine.data-layer.common-test
  (:require [clojure.test :refer [are]]
            [workflo.macros.entity :as e]
            [workflo.query-engine.data-layer :as data-layer]))

(defn test-fetch-one
  [{:keys [connect db data-layer transact resolve-tempid cache]}]
  (println (if cache
             "Test fetching one with cache"
             "Test fetching one without cache"))
  (let [conn (connect)
        layer (data-layer)
        tx (transact conn)
        resolve-id (partial resolve-tempid conn tx)]
    (are [args result]
        (= result
           (let [[entity id params attrs] args]
             (data-layer/fetch-one layer
                                   {:db (db conn)
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

(defn test-fetch-many
  [{:keys [connect db data-layer transact resolve-tempid cache]}]
  (println (if cache
             "Test fetching many with cache"
             "Test fetching many without cache"))
  (let [conn (connect)
        layer (data-layer)
        tx (transact conn)
        resolve-id (partial resolve-tempid conn tx)]
    (are [args result]
        (= result
           (let [[entity ids params attrs] args]
             (data-layer/fetch-many layer
                                    {:db (db conn)
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
                                        {:db/id (resolve-id -1003)}]}}

      ;; Fetch instaces with sorting (ascending)
      ['component-library [(resolve-id -100) (resolve-id -101)
                           (resolve-id -102)]
       {:sort/attr :component-library/name}
       [:component-library/name]]
      [{:component-library/name "Event Site Components"}
       {:component-library/name "Shop Components"}
       {:component-library/name "Social Network Components"}]

      ;; Fetch instaces with sorting (descending)
      ['component-library [(resolve-id -100) (resolve-id -101)
                           (resolve-id -102)]
       {:sort/attr :component-library/name
        :sort/order :sort/descending}
       [:component-library/name]]
      [{:component-library/name "Social Network Components"}
       {:component-library/name "Shop Components"}
       {:component-library/name "Event Site Components"}]

      ;; Fetch instances with pagination
      ['component (mapv resolve-id [-1000 -1001 -1002 -1003 -1004])
       {:sort/attr :component/name
        :paginate/after-index -1
        :paginate/count 3}
       [:db/id :component/name]]
      [{:db/id (resolve-id -1001) :component/name "Cart Info"}
       {:db/id (resolve-id -1003) :component/name "Dislike Button"}
       {:db/id (resolve-id -1002) :component/name "Like Button"}]

      ;; Fetch another "page"
      ['component (mapv resolve-id [-1000 -1001 -1002 -1003 -1004])
       {:sort/attr :component/name
        :paginate/after-index 2
        :paginate/count 3}
       [:db/id :component/name]]
      [{:db/id (resolve-id -1004) :component/name "Seat Picker"}
       {:db/id (resolve-id -1000) :component/name "Shop Item"}])))

(defn test-fetch-all
  [{:keys [connect db data-layer transact resolve-tempid cache]}]
  (println (if cache
             "Test fetching all with cache"
             "Test fetching all without cache"))
  (let [conn (connect)
        layer (data-layer)
        tx (transact conn)
        resolve-id (partial resolve-tempid conn tx)]
    (are [args result]
        (= result
           (let [[entity params attrs] args]
             (data-layer/fetch-all layer
                                   {:db (db conn)
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
         :component-library/components [{:db/id (resolve-id -1004)}]}}

      ;; Fetch instaces with sorting (ascending)
      ['component-library {:sort/attr :component-library/name}
       [:component-library/name]]
      [{:component-library/name "Event Site Components"}
       {:component-library/name "Shop Components"}
       {:component-library/name "Social Network Components"}]

      ;; Fetch instaces with sorting (descending)
      ['component-library {:sort/attr :component-library/name
                           :sort/order :sort/descending}
       [:component-library/name]]
      [{:component-library/name "Social Network Components"}
       {:component-library/name "Shop Components"}
       {:component-library/name "Event Site Components"}]

      ;; Fetch instances with pagination
      ['component {:sort/attr :component/name
                   :paginate/after-index -1
                   :paginate/count 3}
       [:db/id :component/name]]
      [{:db/id (resolve-id -1001) :component/name "Cart Info"}
       {:db/id (resolve-id -1003) :component/name "Dislike Button"}
       {:db/id (resolve-id -1002) :component/name "Like Button"}]

      ;; Fetch another "page"
      ['component {:sort/attr :component/name
                   :paginate/after-index 2
                   :paginate/count 3}
       [:db/id :component/name]]
      [{:db/id (resolve-id -1004) :component/name "Seat Picker"}
       {:db/id (resolve-id -1000) :component/name "Shop Item"}])))

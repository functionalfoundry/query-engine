(ns workflo.query-engine.data-layer.common-test
  (:require [clojure.test :refer [are]]
            [workflo.macros.entity :as e]
            [workflo.macros.query.om-next :as query.om-next]
            [workflo.query-engine.core :as qe]
            [workflo.query-engine.data-layer :as data-layer]))

(defn test-fetch-one
  [{:keys [connect db data-layer transact resolve-tempid
           new-cache cache?]}]
  (println (if cache?
             "Test fetching one with cache"
             "Test fetching one without cache"))
  (let [conn (connect)
        layer (data-layer)
        tx (transact conn)
        resolve-id (partial resolve-tempid conn tx)
        shared-cache (new-cache)]
    (are [args result]
        (= result
           (let [{:keys [viewer entity
                         id params attrs
                         empty-cache?]} args]
             (e/configure-entities!
              {:auth-query (fn [{:keys [db]} query]
                             (let [query' (query.om-next/query query)]
                               (qe/query query' layer
                                         {:db db
                                          :cache (new-cache)
                                          :viewer viewer
                                          :skip-authorization? true})))})
             (data-layer/fetch-one layer
                                   {:db (db conn)
                                    :cache (if empty-cache?
                                             (new-cache)
                                             shared-cache)
                                    :viewer viewer}
                                   (e/resolve-entity entity)
                                   id params attrs)))

      ;; Fetch account once
      {:viewer  (resolve-id -10)
       :entity 'account
       :id (resolve-id -1)
       :params {}
       :attrs [:account/name]}
      {:account/name "Company A"}

      ;; Fetch account once more with the same cache to ensure
      ;; it still works (the cache might break it)
      {:viewer (resolve-id -10)
       :entity 'account
       :id (resolve-id -1)
       :params {}
       :attrs [:account/name]}
      {:account/name "Company A"}

      ;; Fetch the same account again but this time with
      ;; more attributes
      {:viewer (resolve-id -10)
       :entity 'account
       :id (resolve-id -1)
       :params {}
       :attrs [:db/id :account/name :account/users]}
      {:db/id (resolve-id -1)
       :account/name "Company A"
       :account/users [{:db/id (resolve-id -10)}
                       {:db/id (resolve-id -11)}]}

      ;; Fetch the same account but as an unauthorized viewer; for
      ;; this we'll need a fresh cache as each cache is only consistent
      ;; for a single viewer
      {:viewer (resolve-id -12)
       :entity 'account
       :id (resolve-id -1)
       :params {}
       :attrs [:db/id :account/name :account/users]
       :empty-cache? true}
      nil

      ;; Fetch a different entity
      {:viewer (resolve-id -10)
       :entity 'component-library
       :id (resolve-id -100)
       :params {}
       :attrs [:component-library/name
               :component-library/account
               :component-library/creator
               :component-library/components]}
      {:component-library/name "Shop Components"
       :component-library/account {:db/id (resolve-id -1)}
       :component-library/creator {:db/id (resolve-id -10)}
       :component-library/components [{:db/id (resolve-id -1000)}
                                      {:db/id (resolve-id -1001)}]}

      ;; Fetch a non-public component as an otherwise unauthorized user
      {:viewer (resolve-id -12)
       :entity 'component
       :id (resolve-id -1002)
       :params {}
       :attrs [:component/name]
       :empty-cache? true}
      nil

      ;; Fetch a public component as an otherwise unauthorized user
      {:viewer (resolve-id -12)
       :entity 'component
       :id (resolve-id -1003)
       :params {}
       :attrs [:component/name]
       :empty-cache? true}
      {:component/name "Dislike Button"}

      ;; Use deep parameterization to query the account that has a
      ;; specific :db/id
      {:viewer (resolve-id -10)
       :entity 'account
       :id (resolve-id -1)
       :params {[:db/id] (resolve-id -1)}
       :attrs [:db/id :account/name]
       :empty-cache? true}
      {:db/id (resolve-id -1)
       :account/name "Company A"}

      ;; Use deep parameterization to query the account that includes
      ;; a component library with a specific :db/id
      {:viewer (resolve-id -10)
       :entity 'account
       :id (resolve-id -1)
       :params {[:account/libraries
                 :db/id] (resolve-id -100)}
       :attrs [:db/id :account/name]
       :empty-cache? true}
      {:db/id (resolve-id -1)
       :account/name "Company A"}

      ;; Use deep parameterization to query the account that includes
      ;; a component library with a component that has a specific name
      {:viewer (resolve-id -10)
       :entity 'account
       :id (resolve-id -1)
       :params {[:account/libraries
                 :component-library/components
                 :component/name] "Like Button"}
       :attrs [:db/id :account/name]
       :empty-cache? true}
      {:db/id (resolve-id -1)
       :account/name "Company A"}

      ;; Use deep parameterization to query the account that includes
      ;; a component library with the given ID and that has the
      ;; account name "Company A"
      {:viewer (resolve-id -10)
       :entity 'account
       :id (resolve-id -1)
       :params {[:account/libraries :db/id] (resolve-id -100)
                :account/name "Company A"}
       :attrs [:db/id :account/name]
       :empty-cache? true}
      {:db/id (resolve-id -1)
       :account/name "Company A"}

      ;; Use deep parameterization to query the account that includes
      ;; a component library with the given ID and that has the
      ;; account name "Company B"
      {:viewer (resolve-id -10)
       :entity 'account
       :id (resolve-id -1)
       :params {[:account/libraries :db/id] (resolve-id -100)
                :account/name "Company B"}
       :attrs [:db/id :account/name]
       :empty-cache? true}
      nil)))

(defn test-fetch-many
  [{:keys [connect db data-layer transact resolve-tempid
           new-cache cache?]}]
  (println (if cache?
             "Test fetching many with cache"
             "Test fetching many without cache"))
  (let [conn (connect)
        layer (data-layer)
        tx (transact conn)
        resolve-id (partial resolve-tempid conn tx)
        shared-cache (new-cache)]
    (are [args result]
        (= result
           (let [{:keys [viewer entity
                         ids params attrs
                         empty-cache?]} args]
             (e/configure-entities!
              {:auth-query (fn [{:keys [db]} query]
                             (let [query' (query.om-next/query query)]
                               (qe/query query' layer
                                         {:db db
                                          :cache (new-cache)
                                          :viewer viewer
                                          :skip-authorization? true})))})
             (data-layer/fetch-many layer
                                    {:db (db conn)
                                     :cache (if empty-cache?
                                              (new-cache)
                                              shared-cache)
                                     :viewer viewer}
                                    (e/resolve-entity entity)
                                    ids params attrs)))

      ;; Fetch accounts once
      {:viewer (resolve-id -10)
       :entity 'account
       :ids [(resolve-id -1)]
       :params {}
       :attrs [:account/name]}
      #{{:account/name "Company A"}}

      ;; Fetch multiple accounts (but only being authorized to
      ;; see the first)
      {:viewer (resolve-id -10)
       :entity 'account
       :ids [(resolve-id -1) (resolve-id -2)]
       :params {}
       :attrs [:account/name]}
      #{{:account/name "Company A"}}

      ;; Fetch multiple accounts with more attributes
      {:viewer (resolve-id -10)
       :entity 'account
       :ids [(resolve-id -1) (resolve-id -2)]
       :params {}
       :attrs [:db/id :account/name :account/users]}
      #{{:db/id (resolve-id -1)
         :account/name "Company A"
         :account/users [{:db/id (resolve-id -10)}
                         {:db/id (resolve-id -11)}]}}

      ;; Fetch instances of a different entity
      {:viewer (resolve-id -11)
       :entity 'component-library
       :ids [(resolve-id -100) (resolve-id -101)]
       :params {}
       :attrs [:component-library/name
               :component-library/account
               :component-library/creator
               :component-library/components]}
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

      ;; Fetch instances of entities with multiple authorization rules
      ;; In this example: the user -21 is authorized explicitly only
      ;; for component -1004 but since -1003 is public, it is alos
      ;; included
      {:viewer (resolve-id -12)
       :entity 'component
       :ids (mapv resolve-id [-1000 -1001 -1002 -1003 -1004])
       :params {}
       :attrs [:component/name]
       :empty-cache? true}
      #{{:component/name "Dislike Button"}
        {:component/name "Seat Picker"}}

      ;; Fetch instaces with sorting (ascending); does not return
      ;; the unauthorized item -1004
      {:viewer (resolve-id -10)
       :entity 'component
       :ids (mapv resolve-id [-1000 -1001 -1002 -1003 -1004])
       :params {:sort/attr :component/name}
       :attrs [:component/name]}
      [{:component/name "Cart Info"}
       {:component/name "Dislike Button"}
       {:component/name "Like Button"}
       {:component/name "Shop Item"}]

      ;; Fetch instaces with sorting (descending)
      {:viewer (resolve-id -10)
       :entity 'component
       :ids (mapv resolve-id [-1000 -1001 -1002 -1003 -1004])
       :params {:sort/attr :component/name
                :sort/order :sort/descending}
       :attrs [:component/name]}
      [{:component/name "Shop Item"}
       {:component/name "Like Button"}
       {:component/name "Dislike Button"}
       {:component/name "Cart Info"}]

      ;; Fetch instances with pagination
      {:viewer (resolve-id -10)
       :entity 'component
       :ids (mapv resolve-id [-1000 -1001 -1002 -1003 -1004])
       :params {:sort/attr :component/name
                :page/count 3}
       :attrs [:db/id :component/name]}
      [{:db/id (resolve-id -1001) :component/name "Cart Info"}
       {:db/id (resolve-id -1003) :component/name "Dislike Button"}
       {:db/id (resolve-id -1002) :component/name "Like Button"}]

      ;; Fetch another "page"
      {:viewer (resolve-id -10)
       :entity 'component
       :ids (mapv resolve-id [-1000 -1001 -1002 -1003 -1004])
       :params {:sort/attr :component/name
                :page/after-id (resolve-id -1002)
                :page/count 3}
       :attrs [:db/id :component/name]}
      [{:db/id (resolve-id -1000) :component/name "Shop Item"}]

      ;; Use deep parameterization to query users that belong
      ;; to the account with the given :db/id
      {:viewer (resolve-id -10)
       :entity 'user
       :ids (mapv resolve-id [-10 -11 -12 -13 -14])
       :params {[:user/account :db/id] (resolve-id -1)}
       :attrs [:user/name :user/account]
       :empty-cache? true}
      #{{:user/name "Joe" :user/account {:db/id (resolve-id -1)}}
        {:user/name "Jeff" :user/account {:db/id (resolve-id -1)}}}

      ;; Use deep parameterization to query users that belong
      ;; to the account with the given account name
      {:viewer (resolve-id -10)
       :entity 'user
       :ids (mapv resolve-id [-10 -11 -12 -13 -14])
       :params {[:user/account :account/name] "Company A"}
       :attrs [:user/name :user/account]
       :empty-cache? true}
      #{{:user/name "Joe" :user/account {:db/id (resolve-id -1)}}
        {:user/name "Jeff" :user/account {:db/id (resolve-id -1)}}}

      ;; Use deep parameterization to query users that belong
      ;; to the account with the given account name and that
      ;; have the user name "Joe"
      {:viewer (resolve-id -10)
       :entity 'user
       :ids (mapv resolve-id [-10 -11 -12 -13 -14])
       :params {[:user/account :account/name] "Company A"
                :user/name "Joe"}
       :attrs [:user/name :user/account]
       :empty-cache? true}
      #{{:user/name "Joe" :user/account {:db/id (resolve-id -1)}}})))

(defn test-fetch-all
  [{:keys [connect db data-layer transact resolve-tempid
           new-cache cache?]}]
  (println (if cache?
             "Test fetching all with cache"
             "Test fetching all without cache"))
  (let [conn (connect)
        layer (data-layer)
        tx (transact conn)
        resolve-id (partial resolve-tempid conn tx)
        shared-cache (new-cache)]
    (are [args result]
        (= result
           (let [{:keys [viewer entity
                         id params attrs
                         empty-cache?]} args]
             (e/configure-entities!
              {:auth-query (fn [{:keys [db]} query]
                             (let [query' (query.om-next/query query)]
                               (qe/query query' layer
                                         {:db db
                                          :cache (new-cache)
                                          :viewer viewer
                                          :skip-authorization? true})))})
             (data-layer/fetch-all layer
                                   {:db (db conn)
                                    :cache (if empty-cache?
                                             (new-cache)
                                             shared-cache)
                                    :viewer viewer}
                                   (e/resolve-entity entity)
                                   params attrs)))

      ;; Fetch all accounts as user -10
      {:viewer (resolve-id -10)
       :entity 'account
       :params {}
       :attrs [:account/name]}
      #{{:account/name "Company A"}}

      ;; Fetch all accounts as user -10 again to test that caching
      ;; doesn't break things
      {:viewer (resolve-id -10)
       :entity 'account
       :params {}
       :attrs [:account/name]}
      #{{:account/name "Company A"}}

      ;; Fetch all accounts as user -12 using a different cache
      {:viewer (resolve-id -12)
       :entity 'account
       :params {}
       :attrs [:account/name]
       :empty-cache? true}
      #{{:account/name "Company B"}}

      ;; Fetch all accounts with more attributes
      {:viewer (resolve-id -10)
       :entity 'account
       :params {}
       :attrs [:db/id :account/name :account/users]}
      #{{:db/id (resolve-id -1)
         :account/name "Company A"
         :account/users [{:db/id (resolve-id -10)}
                         {:db/id (resolve-id -11)}]}}

      ;; Fetch all component libraries, including the public
      ;; "Event Site Components" library
      {:viewer (resolve-id -10)
       :entity 'component-library
       :params {}
       :attrs [:component-library/name
               :component-library/account
               :component-library/creator
               :component-library/components]}
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
      {:viewer (resolve-id -10)
       :entity 'component-library
       :params {:sort/attr :component-library/name}
       :attrs [:component-library/name]}
      [{:component-library/name "Event Site Components"}
       {:component-library/name "Shop Components"}
       {:component-library/name "Social Network Components"}]

      ;; Fetch instaces with sorting (descending)
      {:viewer (resolve-id -10)
       :entity 'component-library
       :params {:sort/attr :component-library/name
                :sort/order :sort/descending}
       :attrs [:component-library/name]}
      [{:component-library/name "Social Network Components"}
       {:component-library/name "Shop Components"}
       {:component-library/name "Event Site Components"}]

      ;; Fetch instances with pagination
      {:viewer (resolve-id -10)
       :entity 'component
       :params {:sort/attr :component/name
                :page/count 3}
       :attrs [:db/id :component/name]}
      [{:db/id (resolve-id -1001) :component/name "Cart Info"}
       {:db/id (resolve-id -1003) :component/name "Dislike Button"}
       {:db/id (resolve-id -1002) :component/name "Like Button"}]

      ;; Fetch another "page"
      {:viewer (resolve-id -10)
       :entity 'component
       :params {:sort/attr :component/name
                :page/after-id (resolve-id -1002)
                :page/count 3}
       :attrs [:db/id :component/name]}
      [{:db/id (resolve-id -1000) :component/name "Shop Item"}]

      ;; Use deep parameterization to query all users that belong
      ;; to the account with the given :db/id
      {:viewer (resolve-id -10)
       :entity 'user
       :params {[:user/account :db/id] (resolve-id -1)}
       :attrs [:user/name :user/account]
       :empty-cache? true}
      #{{:user/name "Joe" :user/account {:db/id (resolve-id -1)}}
        {:user/name "Jeff" :user/account {:db/id (resolve-id -1)}}}

      ;; Use deep parameterization to query users that belong
      ;; to the account with the given account name
      {:viewer (resolve-id -10)
       :entity 'user
       :params {[:user/account :account/name] "Company A"}
       :attrs [:user/name :user/account]}
      #{{:user/name "Joe" :user/account {:db/id (resolve-id -1)}}
        {:user/name "Jeff" :user/account {:db/id (resolve-id -1)}}}

      ;; Use deep parameterization to query all users that belong
      ;; to the account with the given account name and that
      ;; have the user name "Joe"
      {:viewer (resolve-id -10)
       :entity 'user
       :params {[:user/account :account/name] "Company A"
                :user/name "Joe"}
       :attrs [:user/name :user/account]
       :empty-cache? true}
      #{{:user/name "Joe" :user/account {:db/id (resolve-id -1)}}})))

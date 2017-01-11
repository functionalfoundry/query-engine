(ns workflo.query-engine.query.common-test
  (:require [clojure.test :refer [are]]
            [workflo.macros.entity :as e]
            [workflo.macros.query.om-next :as query.om-next]
            [workflo.query-engine.core :as qe]
            [clojure.zip :as zip]))

(defn test-process-queries
  [{:keys [connect db data-layer transact resolve-tempid
           new-cache cache?]}]
  (println (if cache?
             "Test processing queries with cache"
             "Test processing queries without cache"))
  (let [conn (connect)
        layer (data-layer)
        tx (transact conn)
        resolve-id (partial resolve-tempid conn tx)
        shared-cache (new-cache)]
    (are [args result]
        (= result
           (let [{:keys [viewer query empty-cache?
                         query-hooks]} args]
             (e/configure-entities!
              {:auth-query (fn [{:keys [db]} query]
                             (let [query' (query.om-next/query query)]
                               (qe/query query' layer
                                         {:db db
                                          :viewer viewer
                                          :skip-authorization? true})))})
             (qe/query query layer
                       {:db (db conn)
                        :cache (if empty-cache?
                                 (new-cache)
                                 shared-cache)
                        :viewer viewer}
                       {:query-hooks query-hooks})))

      ;; Query components via a top-level keyword; all
      ;; accessible components are returned with their
      ;; :db/id values, as no attributes are specified
      ;; in the query
      {:query [:components]
       :viewer (resolve-id -10)}

      {:components #{{:db/id (resolve-id -1000)}
                     {:db/id (resolve-id -1001)}
                     {:db/id (resolve-id -1002)}
                     {:db/id (resolve-id -1003)}}}

      ;; Query a specific component via a ident; it is returned
      ;; only with its :db/id value, since no attributes are
      ;; specified in the query
      {:query [[:component (resolve-id -1000)]]
       :viewer (resolve-id -10)}

      {:component {:db/id (resolve-id -1000)}}

      ;; Query an account with no library references and verify
      ;; that an empty set of references is returned rather than
      ;; nil
      {:query [{[:account (resolve-id -3)]
                [:account/name
                 {:account/users [:db/id]}
                 {:account/libraries [:db/id]}]}]
       :viewer (resolve-id -13)}

      {:account {:account/name "Company C"
                 :account/users #{{:db/id (resolve-id -13)}}
                 :account/libraries #{}}}

      ;; Query for components with a keyword join
      {:query [{:components [:component/name]}]
       :viewer (resolve-id -10)}

      {:components
       #{{:component/name "Shop Item"}
         {:component/name "Cart Info"}
         {:component/name "Like Button"}
         {:component/name "Dislike Button"}}}

      ;; Query all accounts (implictly including its :db/id attribute)
      ;; and all components with their names
      {:query [:accounts {:components [:component/name]}]
       :viewer (resolve-id -10)}

      {:accounts #{{:db/id (resolve-id -1)}}
       :components
       #{{:component/name "Shop Item"}
         {:component/name "Cart Info"}
         {:component/name "Like Button"}
         {:component/name "Dislike Button"}}}

      ;; Query for a specific component with a ident join
      {:query [{[:component (resolve-id -1000)]
                [:db/id :component/name :component/account]}]
       :viewer (resolve-id -10)}

      {:component {:db/id (resolve-id -1000)
                   :component/name "Shop Item"
                   :component/account {:db/id (resolve-id -1)}}}

      ;; Query all components with a keyword join
      ;; as a different viewer
      {:query [{:components [:component/name]}]
       :viewer (resolve-id -12)
       :empty-cache? true}

      {:components
       #{{:component/name "Dislike Button"}
         {:component/name "Seat Picker"}}}

      ;; Query for a specific component with a ident join
      ;; as an unauthorized user
      {:query [{[:component (resolve-id -1000)]
                [:db/id :component/name :component/account]}]
       :viewer (resolve-id -12)
       :empty-cache? true}

      {:component nil}

      ;; Query all components and sort by their names using
      ;; a parameterized query
      {:query '[({:components [:component/name]}
                 {:sort/attr :component/name})]
       :viewer (resolve-id -10)}

      {:components
       [{:component/name "Cart Info"}
        {:component/name "Dislike Button"}
        {:component/name "Like Button"}
        {:component/name "Shop Item"}]}

      ;; Query all components and sort in reverse order
      ;; by their names using a parameterized query
      {:query '[({:components [:component/name]}
                 {:sort/attr :component/name
                  :sort/order :sort/descending})]
       :viewer (resolve-id -10)}

      {:components
       [{:component/name "Shop Item"}
        {:component/name "Like Button"}
        {:component/name "Dislike Button"}
        {:component/name "Cart Info"}]}

      ;; Query for one component and filter by :db/id
      {:query `[(:component {:db/id ~(resolve-id -1000)})]
       :viewer (resolve-id -10)}

      {:component {:db/id (resolve-id -1000)}}

      ;; Query for all component and filter by :db/id
      {:query `[(:components {:db/id ~(resolve-id -1000)})]
       :viewer (resolve-id -10)}

      {:components #{{:db/id (resolve-id -1000)}}}

      ;; Query for all components and parameterize the join
      ;; with their component states to sort their names
      ;; descendingly
      {:query '[{:components
                 [:component/name
                  ({:component/states [:component-state/name]}
                   {:sort/attr :component-state/name
                    :sort/order :sort/descending})]}]
       :viewer (resolve-id -10)}

      {:components
       #{{:component/name "Dislike Button"
          :component/states
          [{:component-state/name "Dislike Button Regular"}
           {:component-state/name "Dislike Button Active"}]}
         {:component/name "Cart Info"
          :component/states #{}}
         {:component/name "Like Button"
          :component/states
          [{:component-state/name "Like Button Regular"}
           {:component-state/name "Like Button Active"}]}
         {:component/name "Shop Item"
          :component/states
          [{:component-state/name "Shop Item Selected"}
           {:component-state/name "Shop Item Regular"}]}}}

      ;; Query for all components and parameterize the join
      ;; with their component states to sort their names
      ;; descendingly, this time also sort the components
      {:query '[({:components
                  [:component/name
                   ({:component/states [:component-state/name]}
                    {:sort/attr :component-state/name
                     :sort/order :sort/descending})]}
                 {:sort/attr :component/name
                  :sort/order :sort/descending})]
       :viewer (resolve-id -10)}

      {:components
       [{:component/name "Shop Item"
         :component/states
         [{:component-state/name "Shop Item Selected"}
          {:component-state/name "Shop Item Regular"}]}
        {:component/name "Like Button"
         :component/states
         [{:component-state/name "Like Button Regular"}
          {:component-state/name "Like Button Active"}]}
        {:component/name "Dislike Button"
         :component/states
         [{:component-state/name "Dislike Button Regular"}
          {:component-state/name "Dislike Button Active"}]}
        {:component/name "Cart Info"
         :component/states #{}}]}

      ;; Query one component by ID, join in its component states
      ;; and their component (which should be the same component
      ;; again)
      {:query `[({:component [:component/name
                              {:component/states
                               [:component-state/name
                                {:component-state/component
                                 [:component/name]}]}]}
                 {:db/id ~(resolve-id -1000)})]
       :viewer (resolve-id -10)}

      {:component
       {:component/name "Shop Item"
        :component/states
        #{{:component-state/name "Shop Item Regular"
           :component-state/component
           {:component/name "Shop Item"}}
          {:component-state/name "Shop Item Selected"
           :component-state/component
           {:component/name "Shop Item"}}}}}

      ;; Query all components plus a specific account and user in each
      ;; component query via an ident
      {:query [{:components [:db/id
                             [:account (resolve-id -1)]
                             [:user (resolve-id -10)]]}]
       :viewer (resolve-id -10)}

      {:components
       #{{:db/id (resolve-id -1000)
          :account {:db/id (resolve-id -1)}
          :user {:db/id (resolve-id -10)}}
         {:db/id (resolve-id -1001)
          :account {:db/id (resolve-id -1)}
          :user {:db/id (resolve-id -10)}}
         {:db/id (resolve-id -1002)
          :account {:db/id (resolve-id -1)}
          :user {:db/id (resolve-id -10)}}
         {:db/id (resolve-id -1003)
          :account {:db/id (resolve-id -1)}
          :user {:db/id (resolve-id -10)}}}}

      ;; Query a specific component plus a specific account's
      ;; information via ident joins
      {:query [{[:component (resolve-id -1000)]
                [:component/name {[:account (resolve-id -1)]
                                  [:account/name :account/users]}]}]
       :viewer (resolve-id -10)}

      {:component
       {:component/name "Shop Item"
        :account {:account/name "Company A"
                  :account/users [{:db/id (resolve-id -10)}
                                  {:db/id (resolve-id -11)}]}}}

      ;; Query the component that contains a specific
      ;; component state
      {:query `[({:component [:component/name :component/states]}
                 {:component/states ~(resolve-id -10004)})]
       :viewer (resolve-id -10)}

      {:component
       {:component/name "Dislike Button"
        :component/states [{:db/id (resolve-id -10004)}
                           {:db/id (resolve-id -10005)}]}}

      ;; Query all components that were created by a
      ;; specific user (the viewer)
      {:query `[({:components [:component/name :component/creator]}
                 {:component/creator ~(resolve-id -10)})]
       :viewer (resolve-id -10)}

      {:components
       #{{:component/name "Shop Item"
          :component/creator {:db/id (resolve-id -10)}}
         {:component/name "Like Button"
          :component/creator {:db/id (resolve-id -10)}}}}

      ;; Query a key that directly corresponds to a query hook
      {:query [:foo]
       :query-hooks {:foo (fn [env parent z params] :bar)}
       :viewer (resolve-id -10)}

      {:foo :bar}

      ;; Query using a top-level join that directly corresponds to
      ;; a query hook
      {:query [{:foo [:bar :baz]}]
       :query-hooks {:foo (fn [env parent z params]
                            (zipmap (zip/node z)
                                    (map name (zip/node z))))}
       :viewer (resolve-id -10)}

      {:foo {:bar "bar" :baz "baz"}}

      ;; Query all components with their name and a non-existent
      ;; attribute that triggers a query hook
      {:query [{:components [:component/name :extra-info]}]
       :query-hooks {:extra-info (fn [env parent z params] :foo)}
       :viewer (resolve-id -12)}

      {:components
       #{{:component/name "Dislike Button" :extra-info :foo}
         {:component/name "Seat Picker" :extra-info :foo}}}

      ;; Query all components with their name and a join via
      ;; a non-existent attribute that triggers a query hook
      ;; to include arbitrary data in the result
      {:query [{:components [:component/name
                             {:extra-info [:a :b]}]}]
       :query-hooks {:extra-info (fn [env parent z params]
                                   {:a :foo :b :bar})}
       :viewer (resolve-id -12)}

      {:components
       #{{:component/name "Dislike Button"
          :extra-info {:a :foo :b :bar}}
         {:component/name "Seat Picker"
          :extra-info {:a :foo :b :bar}}}}

      ;; Query components with a join that triggers a hook; verify
      ;; that if the hook sets the :stop-processing? meta flag, the
      ;; query isn't processed beyond the hook
      {:query [{:components [:component/name
                             {:extra [{:foo :bar}
                                      {:bar :baz}]}]}]
       :query-hooks {:extra (fn [env parent z params]
                              (with-meta
                                {:a :b}
                                {:stop-processing? true}))}
       :viewer (resolve-id -12)}

      {:components
       #{{:component/name "Dislike Button" :extra {:a :b}}
         {:component/name "Seat Picker" :extra {:a :b}}}}

      ;; Query the accounts of users via a singular backref; due to
      ;; the singular backref and there being only a single account
      ;; in the :account/_users result, the data in :account/_users
      ;; becomes a single entity instead of a set
      {:query [{:users [:db/id :user/name
                        {:account/_users [:account/name
                                          {:account/users [:user/name]}]}]}]
       :viewer (resolve-id -10)}
      {:users #{{:db/id (resolve-id -10)
                 :user/name "Joe"
                 :account/_users {:account/name "Company A"
                                  :account/users #{{:user/name "Joe"}
                                                   {:user/name "Jeff"}}}}
                {:db/id (resolve-id -11)
                 :user/name "Jeff"
                 :account/_users {:account/name "Company A"
                                  :account/users #{{:user/name "Joe"}
                                                   {:user/name "Jeff"}}}}}}

      ;; Query the accounts of users via a plural backref; due to
      ;; the plural backref, the result of :account/_users is turned
      ;; into a set, despite there being only a single account for each
      ;; user
      {:query [{:users [:db/id :user/name
                        {:accounts/_users [:account/name
                                           {:account/users [:user/name]}]}]}]
       :viewer (resolve-id -10)}
      {:users #{{:db/id (resolve-id -10)
                 :user/name "Joe"
                 :accounts/_users #{{:account/name "Company A"
                                     :account/users #{{:user/name "Joe"}
                                                      {:user/name "Jeff"}}}}}
                {:db/id (resolve-id -11)
                 :user/name "Jeff"
                 :accounts/_users #{{:account/name "Company A"
                                     :account/users #{{:user/name "Joe"}
                                                      {:user/name "Jeff"}}}}}}}

      ;; Query the states of components via a singular backref;
      ;; due to this, only the first state is returned in the
      ;; :component-state/_component attribute
      {:query `[{:components
                 [:db/id
                  :component/name
                  ({:component-state/_component [:component-state/name]}
                   {:sort/attr :component-state/name})]}]
       :viewer (resolve-id -10)}
      {:components
       #{{:db/id (resolve-id -1000)
          :component/name "Shop Item"
          :component-state/_component
          {:component-state/name "Shop Item Regular"}}
         {:db/id (resolve-id -1001)
          :component/name "Cart Info"
          :component-state/_component nil}
         {:db/id (resolve-id -1002)
          :component/name "Like Button"
          :component-state/_component
          {:component-state/name "Like Button Active"}}
         {:db/id (resolve-id -1003)
          :component/name "Dislike Button"
          :component-state/_component
          {:component-state/name "Dislike Button Active"}}}}

      ;; Query the the states of components via a pluralized backref;
      ;; This results in all states of a component to be returned as
      ;; a set, under the pluralized attribute name
      {:query [{:components
                [:db/id
                 :component/name
                 {:component-states/_component
                  [:component-state/name]}]}]
       :viewer (resolve-id -10)}
      {:components
       #{{:db/id (resolve-id -1000)
          :component/name "Shop Item"
          :component-states/_component
          #{{:component-state/name "Shop Item Regular"}
            {:component-state/name "Shop Item Selected"}}}
         {:db/id (resolve-id -1001)
          :component/name "Cart Info"
          :component-states/_component #{}}
         {:db/id (resolve-id -1002)
          :component/name "Like Button"
          :component-states/_component
          #{{:component-state/name "Like Button Regular"}
            {:component-state/name "Like Button Active"}}}
         {:db/id (resolve-id -1003)
          :component/name "Dislike Button"
          :component-states/_component
          #{{:component-state/name "Dislike Button Regular"}
            {:component-state/name "Dislike Button Active"}}}}}

      ;; Query with two joins at the same level - this triggers a bug
      ;; where zip/up (or something else) drops the meta data on the
      ;; parent entity when returning from processing the join
      ;; (see meta-before workaround in query.zip/process-join)
      {:query [{:accounts [:account/name
                           {:account/users [:user/name
                                            {:user/account [:account/name]}]}
                           {:account/libraries [:component-library/name]}]}]
       :viewer (resolve-id -10)}
      {:accounts
       #{{:account/name "Company A"
          :account/users
          #{{:user/name "Joe"
             :user/account {:account/name "Company A"}}
            {:user/name "Jeff"
             :user/account {:account/name "Company A"}}}
          :account/libraries
          #{{:component-library/name "Shop Components"}
            {:component-library/name "Social Network Components"}}}}}

      ;; Query a user that has no account; with the join on the unset
      ;; :user/account attribute, the resulting account should be nil,
      ;; not #{} or {}
      {:query [{:user [:user/name {:user/account [:db/id]}]}]
       :viewer (resolve-id -14)}
      {:user {:user/name "Unknown"
              :user/account nil}}

      ;; Use deep parameterization to query all users that belong
      ;; to the account with the given :db/id
      {:query `[({:users [:user/name {:user/account [:account/name]}]}
                 {[:user/account :db/id] ~(resolve-id -1)})]
       :viewer (resolve-id -10)}
      {:users
       #{{:user/name "Joe" :user/account {:account/name "Company A"}}
         {:user/name "Jeff" :user/account {:account/name "Company A"}}}}

      ;; Use deep parameterization to query all users that belong
      ;; to the account with the given name
      {:query '[({:users [:user/name {:user/account [:account/name]}]}
                 {[:user/account :account/name] "Company A"})]
       :viewer (resolve-id -10)}
      {:users
       #{{:user/name "Joe" :user/account {:account/name "Company A"}}
         {:user/name "Jeff" :user/account {:account/name "Company A"}}}}

      ;; Use deep parameterization to query all users that belong
      ;; to the account with the given name and that have the name "Jeff"
      {:query '[({:users [:user/name {:user/account [:account/name]}]}
                 {[:user/account :account/name] "Company A"
                  :user/name "Jeff"})]
       :viewer (resolve-id -10)}
      {:users
       #{{:user/name "Jeff" :user/account {:account/name "Company A"}}}}

      ;; Use deep parameterization to query all users that belong
      ;; to the account with the given name and that have the name "Linda"
      {:query '[({:users [:user/name {:user/account [:account/name]}]}
                 {[:user/account :account/name] "Company A"
                  :user/name "Linda"})]
       :viewer (resolve-id -10)}
      {:users #{}})))

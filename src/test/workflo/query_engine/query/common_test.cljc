(ns workflo.query-engine.query.common-test
  (:require [clojure.test :refer [are]]
            [workflo.macros.entity :as e]
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
                         :query-hooks]} args]
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
          :component/states nil}
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
         :component/states nil}]}

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
          :extra-info {:a :foo :b :bar}}}})))

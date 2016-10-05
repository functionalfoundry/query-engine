(ns workflo.query-engine.query.common-test
  (:require [clojure.test :refer [are]]
            [workflo.macros.entity :as e]
            [workflo.query-engine.query :as q]))

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
           (let [{:keys [viewer query empty-cache?]} args]
             (q/process query {:db (db conn)
                               :data-layer layer
                               :cache (if empty-cache?
                                        (new-cache)
                                        shared-cache)
                               :viewer viewer})))

      ;; Query components via a top-level keyword; all
      ;; accessible components are returned with their
      ;; :db/id values
      {:query [:components]
       :viewer (resolve-id -10)}
      {:components #{{:db/id (resolve-id -1000)}
                     {:db/id (resolve-id -1001)}
                     {:db/id (resolve-id -1002)}
                     {:db/id (resolve-id -1003)}}}

      ;; Query a specific component via a link; it is returned
      ;; only with its :db/id value
      {:query [[:component (resolve-id -1000)]]
       :viewer (resolve-id -10)}
      {:component {:db/id (resolve-id -1000)}}

      ;; Query for components with a keyword join
      {:query [{:components [:component/name]}]
       :viewer (resolve-id -10)}
      {:components #{{:component/name "Shop Item"}
                     {:component/name "Cart Info"}
                     {:component/name "Dislike Button"}
                     {:component/name "Like Button"}}}

      ;; Query for a specific component with a link join
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
      {:components #{{:component/name "Dislike Button"}
                     {:component/name "Seat Picker"}}}

      ;; Query for a specific component with a link join
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
      {:components [{:component/name "Cart Info"}
                    {:component/name "Dislike Button"}
                    {:component/name "Like Button"}
                    {:component/name "Shop Item"}]}

      ;; Query all components and sort in reverse order
      ;; by their names using a parameterized query
      {:query '[({:components [:component/name]}
                 {:sort/attr :component/name
                  :sort/order :sort/descending})]
       :viewer (resolve-id -10)}
      {:components [{:component/name "Shop Item"}
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
      #_(do
        (println "---")
        {:query `[({:component [:component/name
                                {:component/states
                                 [:component-state/name
                                  {:component-state/component
                                   [:component/name]}]}]}
                   {:db/id ~(resolve-id -1000)})]
         :viewer (resolve-id -10)})
      #_{:component
       {:component/name "Shop Item"
        :component/states
        #{{:component-state/name "Shop Item Regular"
           :component-state/component
           {:component/name "Shop Item"}}
          {:component-state/name "Shop Item Selected"
           :component-state/component
           {:component/name "Shop Item"}}}}}

      ;; Query all components with a join and a sub-join for their
      ;; states' names
      {:query [{:components
                [{:component/states
                  [:component-state/name]}]}]
       :viewer (resolve-id -10)}
      {:components
       #{{:component/states
          #{{:component-state/name "Shop Item Regular"}
            {:component-state/name "Shop Item Selected"}}}
         {:component/states
          #{{:component-state/name "Like Button Regular"}
            {:component-state/name "Like Button Active"}}}
         {:component/states
          #{{:component-state/name "Dislike Button Regular"}
            {:component-state/name "Dislike Button Active"}}}
         {:component/states nil}}})))

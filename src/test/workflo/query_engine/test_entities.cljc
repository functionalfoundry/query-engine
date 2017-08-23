(ns workflo.query-engine.test-entities
  (:require [clojure.spec.alpha :as s]
            [workflo.macros.entity :refer [defentity]]
            [workflo.macros.specs.types :as t]))

;;;; Common

(s/def :db/id ::t/id)

;;;; Account

(s/def :account/name (s/and ::t/string ::t/indexed))
(s/def :account/users (t/entity-ref 'user :many? true))
(s/def :account/libraries (t/entity-ref 'component-library :many? true))

(defentity account
  (spec
    (s/keys :req [:db/id
                  :account/name
                  :account/users]
            :opt [:account/libraries]))
  (auth-query
    [({account [db [id]
                account [({users [db [id]]}
                          {db/id ?viewer-id})]]}
      {db/id ?entity-id})])
  (auth
    (and account (seq users))))

;;;; User

(s/def :user/name (s/and ::t/string ::t/indexed))
(s/def :user/email (s/and ::t/string ::t/indexed))
(s/def :user/password ::t/string)
(s/def :user/account (s/and (t/entity-ref 'account) ::t/indexed))

(defentity user
  (spec
    (s/keys :req [:db/id
                  :user/name
                  :user/email
                  :user/password]
            :opt [:user/account]))
  (auth-query
    [({user [db [id :as user-id]]}
      {db/id ?entity-id})
     ({account [db [id]
                account [({users [db [id]]}
                          {db/id ?viewer-id})]]}
      {account/users ?entity-id})])
  (auth
    (or (= user-id viewer-id)
        (seq users))))

;;;; Component library

(s/def :component-library/name ::t/string)
(s/def :component-library/account (t/entity-ref 'account))
(s/def :component-library/creator (t/entity-ref 'user))
(s/def :component-library/components
  (t/entity-ref 'component :many? true))
(s/def :component-library/public? ::t/boolean)

(defentity component-library
  (spec
    (s/keys :req [:db/id
                  :component-library/name
                  :component-library/account
                  :component-library/creator]
            :opt [:component-library/components
                  :component-library/public?]))
  (auth-query
    [({user [db [id]
             user [{account [db [id]]} :as viewer-account]]}
      {db/id ?viewer-id})
     ({component-library [component-library [{account [db [id]]} :as lib-account
                                             public?]]}
      {db/id ?entity-id})])
  (auth
    (or (= viewer-account lib-account)
        public?)))

;;;; Component

(s/def :component/name ::t/string)
(s/def :component/account (t/entity-ref 'account))
(s/def :component/creator (t/entity-ref 'user))
(s/def :component/states (t/entity-ref 'component-state :many? true))
(s/def :component/public? ::t/boolean)

(defentity component
  (spec
    (s/keys :req [:db/id
                  :component/name
                  :component/account
                  :component/creator]
            :opt [:component/states
                  :component/public?]))
  (auth-query
    [({user [db [id]
             user [{account [db [id]]} :as viewer-account]]}
      {db/id ?viewer-id})
     ({component [component [{account [db [id]]} :as component-account
                             public?]]}
      {db/id ?entity-id})])
  (auth
    (or (= viewer-account component-account)
        public?)))

;;;; Component state

(s/def :component-state/name ::t/string)
(s/def :component-state/component (t/entity-ref 'component))

(defentity component-state
  (spec
     (s/keys :req [:db/id
                   :component-state/name
                   :component-state/component]))
  (auth-query
    [({user [db [id]
             user [{account [db [id]]} :as viewer-account]]}
      {db/id ?viewer-id})
     ({component [component [{account [db [id]]} :as component-account
                             public?]]}
      {component/states ?entity-id})])
  (auth
    (or (= viewer-account component-account)
        public?)))

;;;; Component tree

(s/def :component-tree/root (t/entity-ref 'tree-component))

(defentity component-tree
  (spec
    (s/keys :req [:db/id
                  :component-tree/root])))

(s/def :tree-component/component (t/entity-ref 'component))
(s/def :tree-component/children (t/entity-ref 'tree-component :many? true))

(defentity tree-component
  (spec
    (s/keys :req [:db/id
                  :tree-component/component]
            :opt [:tree-component/children])))

# workflo/query-engine

[![Clojars Project](https://img.shields.io/clojars/v/workflo/query-engine.svg)](https://clojars.org/workflo/query-engine)
[![Build Status](https://travis-ci.org/functionalfoundry/query-engine.svg?branch=master)](https://travis-ci.org/functionalfoundry/query-engine)

[API docs](https://functionalfoundry.github.io/entitydb/)

The `workflo/query-engine` allows to execute [Om Next](https://github.com/omcljs/om)
queries against different data layers, including:

* [entitydb](https://github.com/functionalfoundry/entitydb)
* [DataScript](https://github.com/tonsky/datascript)
* [Datomic](http://www.datomic.com/)

## Usage (DataScript)

```clojure
(require '[clojure.spec.alpha :as s]
         '[workflo.macros.entity :refer [defentity registered-entities]]
         '[workflo.macros.entity.datascript :refer [entity-schema]]
         '[workflo.macros.specs.types :as types]
         '[workflo.query-engine.core :as query-engine]
         '[workflo.query-engine.data-layer.datascript-no-authorization :as dl])


;; Define a user entity with workflo/macros

(s/def :user/name ::types/string)
(s/def :user/email ::types/string)

(defentity user
  (spec
    (s/keys :req [:db/id
                  :user/name
                  :user/email])))


;; Create a DataScript schema

(def schema (->> (registered-entities)
                 (vals)
                 (map entity-schema)
                 (apply merge)))


;; Create a DataScript database

(def conn (ds/create-conn schema)


;; ... Populate the database with users ...


;; Run a query against the database, fetch all users
;; with their IDs and names

(query-engine/query [{:users [:db/id :user/name]}]
                    (dl/data-layer)
                    {:db @conn})

;; -> #{{:db/id 1 :user/name "John"}
;;      {:db/id 2 :usder/name "Linda"}}
```

## Testing

1. Install [boot](http://boot-clj.com/)
2. Clone this repository
3. Run the tests:
   - `boot test` to run tests once
   - `boot watch test` to run tests continuously on changes

## License

`workflo/query-engine` is copyright (C) 2016-2017 Workflo, Inc.

Licensed under the MIT License.

For more information [see the LICENSE file](LICENSE).

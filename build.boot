#!/usr/bin/env boot


(def +project+ 'workflo/query-engine)
(def +version+ "0.1.30")


(set-env!
 :resource-paths #{"src/main" "resources"}
 :dependencies '[;; Boot
                 [adzerk/boot-cljs "2.0.0" :scope "test"]
                 [adzerk/boot-test "1.2.0" :scope "test"]
                 [adzerk/bootlaces "0.1.13" :scope "test"]
                 [boot-environ "1.1.0" :scope "test"]
                 [boot-codox "0.10.3" :scope "test"]

                 ;; Core
                 [org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/clojurescript "1.9.671"]

                 ;; General
                 [inflections "0.13.0"]
                 [environ "1.1.0"]

                 ;; Data layers
                 [com.datomic/datomic-free "0.9.5561.50" :scope "test"
                  :exclusions [com.google.guava/guava]]
                 [datomic-schema "1.3.0" :scope "test"]
                 [datascript "0.16.1" :scope "test"]

                 ;; Workflo
                 [workflo/entitydb "0.1.6"]
                 [workflo/macros "0.2.63"]])


(require '[adzerk.boot-test :refer [test] :rename {test test-clj}]
         '[adzerk.bootlaces :refer :all]
         '[boot.git :refer [last-commit]]
         '[codox.boot :refer [codox]]
         '[environ.boot :refer [environ]])


(bootlaces! +version+ :dont-modify-paths? true)


(task-options!
 push      {:repo "deploy-clojars"
            :ensure-branch "master"
            :ensure-clean true
            :ensure-tag (last-commit)
            :ensure-version +version+}
 pom       {:project +project+
            :version +version+
            :description "Execute Om Next queries against various data layers"
            :url "https://github.com/workfloapp/query-engine"
            :scm {:url "https://github.com/workfloapp/query-engine"}
            :license {"MIT License" "https://opensource.org/licenses/MIT"}})


(deftask dev-env
  []
  (environ :env {:datomic-uri "datomic:mem://workflo-query-engine"}))


(deftask test-env
  []
  (comp (dev-env)
        (with-pre-wrap fileset
          (merge-env! :source-paths #{"src/test"})
          fileset)))


(deftask test
  []
  (comp (dev-env)
        (test-env)
        (test-clj)))


(deftask dev
  []
  (comp (dev-env)
        (repl)))


(deftask docs
  []
  (comp (codox :name "workflo/query-engine"
               :source-paths #{"src/main"}
               :output-path "docs"
               :metadata {:doc/format :markdown}
               :themes [:default :query-engine])
        (target)))


(deftask install-local
  []
  (comp (pom)
        (jar)
        (install)))


(deftask deploy-snapshot
  []
  (comp (pom)
        (jar)
        (push-snapshot)))


(deftask deploy-release
  []
  (comp (pom)
        (jar)
        (push-release)))

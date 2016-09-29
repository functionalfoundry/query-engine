#!/usr/bin/env boot

(def +project+ 'workflo/query-engine)
(def +version+ "0.1.0-SNAPSHOT")

(set-env!
 :source-paths #{"src/main"}
 :resource-paths #{"resources"}
 :dependencies '[;; Boot
                 [adzerk/boot-test "1.1.2"]
                 [boot-environ "1.1.0"]

                 ;; Core
                 [org.clojure/clojure "1.9.0-alpha11"]

                 ;; General
                 [inflections "0.12.2"]
                 [environ "1.1.0"]

                 [workflo/macros "0.2.16"]])

(require '[adzerk.boot-test :refer :all]
         '[environ.boot :refer [environ]])

(deftask dev-env
  []
  (environ :env {:datomic-uri "datomic:mem://workflo-query-engine"}))

(deftask dev
  []
  (comp (dev-env)
        (repl)))

(ns tools.graphql.sdl-test
  (:require [clojure.data :refer [diff]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [tools.graphql.sdl :as sdl :refer [edn->sdl]]))

(defn read-edn [path]
  (-> (slurp (io/resource path))
      (clojure.edn/read-string)))

(defn diff-edn-sdl [filename]
  (diff (str/join "\n" (edn->sdl (read-edn (str filename ".edn"))))
        (slurp (io/resource (str filename ".graphql")))))

(deftest edn->sdl-test

  (let [test-conversion (fn [filename]
                          (let [[a b _] (diff-edn-sdl filename)]
                            (= a b nil)))]
    (is (test-conversion "enum"))
    (is (test-conversion "interface"))
    (is (test-conversion "object"))
    (is (test-conversion "query"))
    (is (test-conversion "mutation"))
    (is (test-conversion "union"))
    (is (test-conversion "input-object"))
    (is (test-conversion "scalar")))
  )

(comment
  (run-tests)

  @(def sample-enum (read-edn "enum.edn"))
  @(def sample-interface (read-edn "interface.edn"))
  @(def sample-object (read-edn "object.edn"))
  @(def sample-query (read-edn "query.edn"))
  @(def sample-mutation (read-edn "mutation.edn"))
  @(def sample-union (read-edn "union.edn"))
  @(def sample-input-object (read-edn "input-object.edn"))
  @(def sample-scalar (read-edn "scalar.edn"))

  (def sample (merge sample-enum
                     sample-interface
                     sample-object
                     sample-query
                     sample-mutation
                     sample-union
                     sample-input-object
                     sample-scalar))
  (println (str/join "\n" (edn->sdl sample)))
  (println (sdl/->query sample :occupations))
  (println (sdl/->mutation sample :setOccupation))

  )
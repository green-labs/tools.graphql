(ns tools.graphql.sdl-test
  (:require [clojure.data :refer [diff]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [tools.graphql.sdl :refer [edn->sdl]]))

(defn read-edn [path]
  (-> (slurp (io/resource path))
      (edn/read-string)))

(defn diff-edn-sdl [filename]
  (diff (str/join "\n" (edn->sdl (read-edn (str "sdl/" filename ".edn"))))
        (slurp (io/resource (str "sdl/" filename ".graphql")))))

(deftest edn->sdl-test
  (let [test-conversion (fn [filename]
                          (let [[a b _] (diff-edn-sdl filename)]
                            (is (= a b nil))))]
    (test-conversion "enum")
    (test-conversion "enum-simple")
    (test-conversion "interface")
    (test-conversion "object")
    (test-conversion "query")
    (test-conversion "mutation")
    (test-conversion "union")
    (test-conversion "input-object")
    (test-conversion "scalar")
    (test-conversion "input-object-one-of"))) 

(comment
  (run-tests))

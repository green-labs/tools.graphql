(ns tools.graphql.sdl-test
  (:require [clojure.data :refer [diff]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [tools.graphql.sdl :as sdl :refer [edn->sdl ->query ->mutation]]))

(defn read-edn [path]
  (-> (slurp (io/resource path))
      (edn/read-string)))

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
    (is (test-conversion "scalar"))))

(def sample (merge (read-edn "enum.edn")
                   (read-edn "interface.edn")
                   (read-edn "object.edn")
                   (read-edn "query.edn")
                   (read-edn "mutation.edn")
                   (read-edn "union.edn")
                   (read-edn "input-object.edn")
                   (read-edn "scalar.edn")))

(deftest query
  (let [test-conversion (fn [q filename]
                          (= (slurp (io/resource (str filename ".graphql")))
                             (->query sample q {:max-depth 4})))]
    (is (test-conversion :occupations "query1"))
    (is (test-conversion :cardCompanies "query2") "인자가 빈 상태로 생성되는지 확인")
    (is (test-conversion :marketPriceV3LevelNames "query3") "반환값이 기본 타입인 경우")
    (is (test-conversion :communityPinnedPosts "query4") "기본값이 있는 경우")
    (is (test-conversion :me "query5") "field resolver 의 인자가 있는 경우")
    (is (test-conversion :node "query6") "; 인터페이스가 반환되는 경우")))

(deftest mutation
  (let [test-conversion (fn [q filename]
                          (= (slurp (io/resource (str filename ".graphql")))
                             (->mutation sample q)))]
    (is (test-conversion :setOccupation "mutation1") "union 내부에 field resolver 인자가 있는 경우")))

(comment
  (run-tests)

  (println (str/join "\n" (edn->sdl sample)))
  (println (sdl/->query sample :node))
  (println (sdl/->mutation sample :setOccupation)))

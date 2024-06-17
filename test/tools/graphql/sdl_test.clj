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
  (diff (str/join "\n" (edn->sdl (read-edn (str "sdl/" filename ".edn"))))
        (slurp (io/resource (str "sdl/" filename ".graphql")))))

(deftest edn->sdl-test
  (let [test-conversion (fn [filename]
                          (let [[a b _] (diff-edn-sdl filename)]
                            (is (= a b nil))))]
    (test-conversion "enum")
    (test-conversion "interface")
    (test-conversion "object")
    (test-conversion "query")
    (test-conversion "mutation")
    (test-conversion "union")
    (test-conversion "input-object")
    (test-conversion "scalar")))

(def sample (merge (read-edn "sdl/enum.edn")
                   (read-edn "sdl/interface.edn")
                   (read-edn "sdl/object.edn")
                   (read-edn "sdl/query.edn")
                   (read-edn "sdl/mutation.edn")
                   (read-edn "sdl/union.edn")
                   (read-edn "sdl/input-object.edn")
                   (read-edn "sdl/scalar.edn")))

(deftest query
  (let [test-conversion (fn [q filename {:keys [fields-map]
                                         :or   {fields-map nil}}]
                          (= (slurp (io/resource (str "sdl/" filename ".graphql")))
                             (->query sample q {:max-depth  4
                                                :fields-map fields-map})))]
    (is (test-conversion :occupations "query1" nil))
    (is (test-conversion :cardCompanies "query2" nil) "인자가 빈 상태로 생성되는지 확인")
    (is (test-conversion :marketPriceV3LevelNames "query3" nil) "반환값이 기본 타입인 경우")
    (is (test-conversion :communityPinnedPosts "query4" nil) "기본값이 있는 경우")
    (is (test-conversion :me "query5" nil) "field resolver 의 인자가 있는 경우")
    (is (test-conversion :node "query6" nil) "인터페이스가 반환되는 경우")
    (is (test-conversion :nested "query7" {:fields-map {:nested {:a nil}}}) "fields-map 옵션 테스트 1")
    (is (test-conversion :nested "query8" {:fields-map {:nested {:b nil}}}) "fields-map 옵션 테스트 2")
    (is (test-conversion :nested "query9" {:fields-map {:nested {:b {:c nil}}}}) "fields-map 옵션 테스트 3")))

(deftest mutation
  (let [test-conversion (fn [q filename]
                          (= (slurp (io/resource (str "sdl/" filename ".graphql")))
                             (->mutation sample q)))]
    (is (test-conversion :setOccupation "mutation1") "union 내부에 field resolver 인자가 있는 경우")))

(comment
  (run-tests)

  (mutation)

  (def edn (read-edn (str "sdl/input-object-one-of.edn")))

  (println (str/join "\n" (edn->sdl edn)))

  (edn->sdl (read-edn (str "sdl/input-object-one-of.edn")))
  (diff-edn-sdl "input-object-one-of")

  (println (str/join "\n" (edn->sdl sample)))
  (println (sdl/->query sample :node))
  (println (sdl/->query sample :nested {:fields-map {:nested {:b {:c nil}}}}))
  (println (sdl/->mutation sample :setOccupation)))

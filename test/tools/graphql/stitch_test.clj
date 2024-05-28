(ns tools.graphql.stitch-test
  (:require [clojure.test :refer :all]
            [tools.graphql.stitch.core :as stitch :refer [map->Subschema]]))

(deftest test-conflict
  (let [a (map->Subschema {:name     "a"
                           :contents {:objects {:a 10, :b 20}}})
        b (map->Subschema {:name     "b"
                           :contents {:objects {:a 30}}})
        c (map->Subschema {:name     "c"
                           :contents {:objects {:c 40}}})]
    (testing "same key on objects"
      (is (thrown-with-msg? Exception #"Conflict detected on b"
                            (stitch/stitch-subschemas a b)))
      (is (thrown-with-msg? Exception #"Conflict detected on a"
                            (stitch/stitch-subschemas b a))))

    (testing "no conflict"
      (is (= (:contents (stitch/stitch-subschemas a c))
             {:objects {:a 10, :b 20, :c 40}})))))

(deftest test-stitch-subschemas
  (let [a (map->Subschema {:name     "a"
                           :contents {:objects {:a 10, :b 20}}})
        c (map->Subschema {:name     "c"
                           :contents {:objects {:c 40}}})
        d (map->Subschema {:name     "d"
                           :contents {:objects   {:d 30}
                                      :queries   {:b 20}
                                      :mutations {:c 40}}})]
    (testing "merge"
      (is (= (:contents (reduce stitch/stitch-subschemas [a c d]))
             {:objects {:a 10, :b 20, :c 40, :d 30}, :queries {:b 20}, :mutations {:c 40}}))
      (is (= (:contents (stitch/stitch-subschemas a c))
             {:objects {:a 10, :b 20, :c 40}})))))

(run-tests)
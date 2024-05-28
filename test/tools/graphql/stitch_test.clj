(ns tools.graphql.module-test
  (:require [clojure.test :refer :all]
            [tools.graphql.impl :as module :refer [map->Shard]]))

(deftest test-conflict
  (let [a (map->Shard {:name     "a"
                       :contents {:objects {:a 10, :b 20}}})
        b (map->Shard {:name     "b"
                       :contents {:objects {:a 30}}})
        c (map->Shard {:name     "c"
                       :contents {:objects {:c 40}}})]
    (testing "same key on objects"
      (is (thrown-with-msg? Exception #"Conflict detected on b"
                            (module/merge-shards a b)))
      (is (thrown-with-msg? Exception #"Conflict detected on a"
                            (module/merge-shards b a))))

    (testing "no conflict"
      (is (= (:contents (module/merge-shards a c))
             {:objects {:a 10, :b 20, :c 40}})))))

(deftest test-merge-shards
  (let [a (map->Shard {:name     "a"
                       :contents {:objects {:a 10, :b 20}}})
        c (map->Shard {:name     "c"
                       :contents {:objects {:c 40}}})
        d (map->Shard {:name     "d"
                       :contents {:objects   {:d 30}
                                  :queries   {:b 20}
                                  :mutations {:c 40}}})]
    (testing "merge"
      (is (= (:contents (reduce module/merge-shards [a c d]))
             {:objects {:a 10, :b 20, :c 40, :d 30}, :queries {:b 20}, :mutations {:c 40}}))
      (is (= (:contents (module/merge-shards a c))
             {:objects {:a 10, :b 20, :c 40}})))))

(run-tests)
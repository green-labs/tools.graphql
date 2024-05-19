(ns tools.graphql.validators-test
  (:require [clojure.test :refer :all]
            [tools.graphql.validators :as v]))

(deftest syntax-test
  (testing "type in nested type"
    (let [ts [:User
              '(list :User)
              '(non-null :User)
              '(list (non-null :User))
              '(non-null (list :User))
              '(non-null (list (list :User)))
              '(:User list)]]
      (is (= [true true true true true true false]
             (map v/graphql-type? ts))))))

(deftest no-unreachable-types
  (testing "object not referenced in any queries or mutations"
    (let [schema {:objects   {:User  {:fields {:id        {:type 'String}
                                               :name      {:type 'String}
                                               :email     {:type 'String}
                                               :password  {:type 'String}
                                               :createdAt {:type 'String}
                                               :updatedAt {:type 'String}
                                               :deletedAt {:type :Date}}}
                              :Date  {:fields {:year  {:type 'Int}
                                               :month {:type 'Int}
                                               :day   {:type 'Int}}}
                              :Dummy {:fields {:id {:type 'String}}}}
                  :queries   {:user     {:type :User}
                              :users    {:type '(non-null (list (non-null :User)))}}
                  :mutations {:createUser {:type :User}
                              :updateUser {:type :User}
                              :deleteUser {:type :User}}}]
      (is (= [:Dummy] (v/unreachable-types schema)))))

  (testing "object referenced in union"
    (let [schema {:objects {:UnionMember {:fields {:id {:type 'String}}}}
                  :unions  {:TheUnion {:members [:Pre1 :Pre2 :UnionMember :Post1 :Post2]}}}]
      (is (= [:TheUnion] (v/unreachable-types schema))))))

(deftest no-unreachable-input-types
  (testing "input-object not referenced in any query or mutation"
    (let [schema {:input-objects {:UserInput {:fields {:name     {:type 'String}
                                                       :email    {:type 'String}
                                                       :password {:type 'String}}}
                                  :Dummy     {:fields {:id {:type 'String}}}}
                  :queries       {:user  {:type :User}
                                  :users {:type '(non-null (list (non-null :User)))}}
                  :mutations     {:updateUser {:type :User
                                               :args {:input {:type :UserInput}}}}}]
      (is (= [:Dummy] (v/unreachable-input-types schema)))))

  (testing "input-object referenced in another input-object"
    (let [schema {:input-objects {:SomeInput    {:fields {:id      {:type String}
                                                          :another {:type :AnotherInput}}}
                                  :AnotherInput {:fields {:id {:type String}}}}}]
      (is (= [:SomeInput] (v/unreachable-input-types schema))))))

(run-tests)
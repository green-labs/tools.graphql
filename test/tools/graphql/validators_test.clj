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
                  :queries   {:user  {:type :User}
                              :users {:type '(non-null (list (non-null :User)))}}
                  :mutations {:createUser {:type :User}
                              :updateUser {:type :User}
                              :deleteUser {:type :User}}}]
      (is (= [[:Dummy nil]] (v/unreachable-types schema)))))

  (testing "object referenced in union"
    (let [schema {:objects {:UnionMember {:fields {:id {:type 'String}}}}
                  :unions  {:TheUnion {:members [:Pre1 :Pre2 :UnionMember :Post1 :Post2]}}}]
      (is (= [[:TheUnion nil]] (v/unreachable-types schema))))))

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
      (is (= [[:Dummy nil]] (v/unreachable-input-types schema)))))

  (testing "input-object referenced in another input-object"
    (let [schema {:input-objects {:SomeInput    {:fields {:id      {:type String}
                                                          :another {:type :AnotherInput}}}
                                  :AnotherInput {:fields {:id {:type String}}}}}]
      (is (= [[:SomeInput nil]] (v/unreachable-input-types schema))))))

(deftest no-unreachable-interfaces
  (testing "interface not implemented by any object"
    (let [schema {:interfaces {:Node     {:fields {:id {:type 'String}}}
                               :UnusedIf {:fields {:id {:type 'String}}}}
                  :objects    {:User    {:fields {:id        {:type 'String}
                                                  :name      {:type 'String}
                                                  :email     {:type 'String}
                                                  :password  {:type 'String}
                                                  :createdAt {:type 'String}
                                                  :updatedAt {:type 'String}
                                                  :deletedAt {:type :Date}}}
                               :Nesting {:fields {:id {:type 'String}}}
                               :MyNode  {:implements [:Node]
                                         :fields     {:id {:type 'String}}}
                               :Dummy   {:fields {:id {:type 'String}}}}}]
      (is (= [[:UnusedIf nil]] (v/unreachable-interfaces schema))))))

(deftest interface-with-resolver-test
  (testing "interface should not have any resolvers to its fields"
    (let [schema {:interfaces {:Node {:fields {:id {:type        '(non-null ID)
                                                    :description "The id of the object"}}}
                               :Post {:implements [:Node]
                                      :fields     {:id     {:type        '(non-null ID)
                                                            :description "The id of the object"}
                                                   :title  {:type        '(non-null String)
                                                            :description "The title of the post"
                                                            :resolver    (fn [_ _ _] nil)}
                                                   :author {:type        '(non-null User)
                                                            :description "The author of the post"}}}}}
          result (v/interface-with-resolver schema)]
      (is (= (count result) 1))
      (is (let [[ifc _ field resolver] (first result)]
            (and (= ifc :Post)
                 (= field :title)
                 (some? resolver)))))))

(deftest no-root-resolver
  (testing "query or mutation without resolver"
    (let [schema {:queries   {:user {:type :User}}
                  :mutations {:createUser {:type :User}
                              :updateUser {:type :User}
                              :deleteUser {:type :User}}}]
      (is (= [:user :createUser :updateUser :deleteUser]
             (v/no-root-resolver schema)))))

  (testing "resolver is 'maybe' or nil assigned"
    (let [schema {:queries {:q {:resolve clojure.core/identity}
                            :r {:resolve nil}
                            :s {}}}]
      (is (= [:r :s] (v/no-root-resolver schema))))))

(deftest pagination-direction-test
  (testing "forward direction"
    (is (= :forward (v/pagination-direction '{:first {:type (non-null Int)}
                                              :after {:type String}
                                              :order {:type :MarketPriceV3Order, :default-value :RECENT}})))
    (is (= :forward (v/pagination-direction '{:first {:type Int}
                                              :after {:type String}}))))

  (testing "backward direction"
    (is (= :backward (v/pagination-direction '{:last   {:type (non-null Int)}
                                               :before {:type String}}))))

  (testing "bidirectional direction"
    (is (= :bidirectional (v/pagination-direction '{:first  {:type Int}
                                                    :after  {:type String}
                                                    :last   {:type Int}
                                                    :before {:type String}})))))

(comment
  (pagination-direction-test)
  (run-tests))
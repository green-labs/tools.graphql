(ns tools.graphql.transformers-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [tools.graphql.transformers :refer [relay-pagination]]))

(deftest relay-pagination-test
  (testing "lacinia schema에서 정의한 Node implements 하나만 된 objects들에게 Graphql relay spec에 맞는 edges, connections를 추가해줍니다"
    (let [schema          {:objects {:testObject {:implements  [:Node]
                                                  :pagination  :relay
                                                  :description "test description"
                                                  :fields      {:testField {:description "test field"
                                                                            :type        '(non-null Int)}}}}}
          result          (relay-pagination schema)
          expected-result {:objects {:testObject           {:implements  [:Node]
                                                            :description "test description"
                                                            :fields      {:testField {:description "test field"
                                                                                      :type        '(non-null Int)}}}
                                     :testObjectEdge       {:implements [:Edge]
                                                            :fields     {:cursor {:type        '(non-null String)
                                                                                  :description "Cursor"}
                                                                         :node   {:type        '(non-null :testObject)
                                                                                  :description "Node"}}}
                                     :testObjectConnection {:implements [:Connection]
                                                            :fields     {:edges    {:type        '(non-null (list (non-null :testObjectEdge)))
                                                                                    :description "Edges"}
                                                                         :pageInfo {:type        '(non-null :PageInfo)
                                                                                    :description "Page information"}
                                                                         :count    {:type        '(non-null Int)
                                                                                    :description "Number of edges"}}}}}]
      (is (= result expected-result))))
  (testing "lacinia schema에서 정의한 objects가 Node implements가 아닌 경우에는 아무일도 일어나지 않는다"
    (let [schema          {:objects {:testObject {:description "test description"
                                                  :fields      {:testField {:description "test field"
                                                                            :type        '(non-null Int)}}}}}
          result          (relay-pagination schema)
          expected-result schema]
      (is (= result expected-result))))
  (testing "lacinia schema에서 정의한 objects가 Node와 다른 implements가 같이 있을 때에 Graphql relay spec에 맞는 edges, connections를 추가해줍니다"
    (let [schema          {:objects {:testObject {:implements  [:Person :Node]
                                                  :pagination  :relay
                                                  :description "test description"
                                                  :fields      {:testField {:description "test field"
                                                                            :type        '(non-null Int)}}}}}
          result          (relay-pagination schema)
          expected-result {:objects {:testObject           {:implements  [:Person :Node]
                                                            :description "test description"
                                                            :fields      {:testField {:description "test field"
                                                                                      :type        '(non-null Int)}}}
                                     :testObjectEdge       {:implements [:Edge]
                                                            :fields     {:cursor {:type        '(non-null String)
                                                                                  :description "Cursor"}
                                                                         :node   {:type        '(non-null :testObject)
                                                                                  :description "Node"}}}
                                     :testObjectConnection {:implements [:Connection]
                                                            :fields     {:edges    {:type        '(non-null (list (non-null :testObjectEdge)))
                                                                                    :description "Edges"}
                                                                         :pageInfo {:type        '(non-null :PageInfo)
                                                                                    :description "Page information"}
                                                                         :count    {:type        '(non-null Int)
                                                                                    :description "Number of edges"}}}}}]
      (is (= result expected-result))))
  (testing "lacinia schema에서 objects가 아닌 다른 타입의 경우에는 아무일도 일어나지 않는다"
    (let [schema          {:objects       {:testObject {:implements  [:Node]
                                                        :pagination  :relay
                                                        :description "test description"
                                                        :fields      {:testField {:description "test field"
                                                                                  :type        '(non-null Int)}}}}
                           :enums         {:testEnum {:pagination :relay
                                                      :values     [{:enum-value  :OTHER
                                                                    :description "기타"}]}}
                           :interfaces    {:TestInterface {:pagination :relay
                                                           :fields     {:id {:type '(non-null ID)}}}}
                           :queries       {:testQuery {:pagination :relay
                                                       :type       '(non-null :testObject)
                                                       :resolve    :test-resolver}}
                           :unions        {:testUnion {:pagination :relay
                                                       :members    [:testObject :testEnum]}}
                           :input-objects {:testInputObject {:pagination :relay
                                                             :fields     {:status {:type '(non-null String)}}}}
                           :mutations     {:createRfqRequest {:pagination :relay
                                                              :type       '(non-null :testUnion)
                                                              :args       {:input {:type '(non-null :testInputObject)}}
                                                              :resolve    :test-resolver-1}}}
          result          (relay-pagination schema)
          expected-result (assoc schema :objects {:testObject           {:implements  [:Node]
                                                                         :description "test description"
                                                                         :fields      {:testField {:description "test field"
                                                                                                   :type        '(non-null Int)}}}
                                                  :testObjectEdge       {:implements [:Edge]
                                                                         :fields     {:cursor {:type        '(non-null String)
                                                                                               :description "Cursor"}
                                                                                      :node   {:type        '(non-null :testObject)
                                                                                               :description "Node"}}}
                                                  :testObjectConnection {:implements [:Connection]
                                                                         :fields     {:edges    {:type        '(non-null (list (non-null :testObjectEdge)))
                                                                                                 :description "Edges"}
                                                                                      :pageInfo {:type        '(non-null :PageInfo)
                                                                                                 :description "Page information"}
                                                                                      :count    {:type        '(non-null Int)
                                                                                                 :description "Number of edges"}}}})]
      (is (= result expected-result)))))

(comment
  (run-tests))

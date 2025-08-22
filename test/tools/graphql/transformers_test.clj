(ns tools.graphql.transformers-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [tools.graphql.transformers :refer [relay-pagination transform-directives]]))

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

(deftest transform-directives-test
  (testing "간소화된 directive 문법을 lacinia 형식으로 변환"
    (let [schema {:directive-defs 
                  {:tag {:repeatable true
                         :args {:name {:type '(non-null String)}}}
                   :auth {:args {:roles {:type '(list :Role)}}}}
                  :objects 
                  {:User {:directives {:tag {:name "model"}}
                          :fields {:name {:directives {:tag [{:name "sensitive"} {:name "pii"}]}}
                                   :role {:directives {:auth {:roles [:ADMIN]}}}}}}}
          result (transform-directives schema)
          expected {:directive-defs 
                    {:tag {:repeatable true
                           :args {:name {:type '(non-null String)}}}
                     :auth {:args {:roles {:type '(list :Role)}}}}
                    :objects
                    {:User {:directives [{:directive-type :tag
                                          :directive-args {:name "model"}}]
                            :fields {:name {:directives [{:directive-type :tag
                                                          :directive-args {:name "sensitive"}}
                                                         {:directive-type :tag
                                                          :directive-args {:name "pii"}}]}
                                     :role {:directives [{:directive-type :auth
                                                          :directive-args {:roles [:ADMIN]}}]}}}}}]
      (is (= result expected))))
  
  (testing "기존 lacinia 형식은 그대로 유지"
    (let [schema {:directive-defs {:tag {:repeatable true}}
                  :objects {:User {:directives [{:directive-type :tag
                                                 :directive-args {:name "model"}}]}}}
          result (transform-directives schema)]
      (is (= result schema))))
  
  (testing "repeatable이 아닌 directive에서 리스트 사용시 오류 발생"
    (let [schema {:directive-defs {:auth {:args {:roles {:type '(list :Role)}}}}
                  :objects {:User {:directives {:auth [{:roles "role1"} {:roles "role2"}]}}}}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                           #"Directive :auth is not repeatable"
                           (transform-directives schema)))))
  
  (testing "args가 없는 directive 처리"
    (let [schema {:directive-defs {:skip {:locations #{:field}}}
                  :objects {:User {:fields {:name {:directives {:skip true}}}}}}
          result (transform-directives schema)
          expected-directives (get-in result [:objects :User :fields :name :directives])]
      (is (= expected-directives [{:directive-type :skip}]))))
  
  (testing "빈 맵으로 args 없음 표현"
    (let [schema {:directive-defs {:skip {:locations #{:field}}}
                  :objects {:User {:fields {:name {:directives {:skip {}}}}}}}
          result (transform-directives schema)
          expected-directives (get-in result [:objects :User :fields :name :directives])]
      (is (= expected-directives [{:directive-type :skip}]))))
  
  (testing "repeatable directive에서 일부만 args 있는 경우"
    (let [schema {:directive-defs {:custom {:repeatable true
                                            :args {:name {:type 'String}
                                                   :category {:type 'String}}}}
                  :objects {:User {:directives {:custom [{:name "test"}
                                                         {:category "prod"}
                                                         {}]}}}}
          result (transform-directives schema)
          expected-directives (get-in result [:objects :User :directives])]
      (is (= expected-directives [{:directive-type :custom
                                   :directive-args {:name "test"}}
                                  {:directive-type :custom
                                   :directive-args {:category "prod"}}
                                  {:directive-type :custom}])))))

(comment
  (run-tests))

(ns tools.graphql.validators-test
  (:require [clojure.test :refer :all]
            [tools.graphql.validators :as v]))

(deftest no-unreachable-types
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
                            :users    {:type '(non-null (list (non-null :User)))}
                            :post     {:type :Post}
                            :posts    {:type '(non-null (list (non-null :Post)))}
                            :comment  {:type :Comment}
                            :comments {:type '(non-null (list (non-null :Comment)))}}
                :mutations {:createUser    {:type :User}
                            :updateUser    {:type :User}
                            :deleteUser    {:type :User}
                            :createPost    {:type :Post}
                            :updatePost    {:type :Post}
                            :deletePost    {:type :Post}
                            :createComment {:type :Comment}
                            :updateComment {:type :Comment}
                            :deleteComment {:type :Comment}}}]
    (is (= [:Dummy] (v/unreachable-types schema)))))


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
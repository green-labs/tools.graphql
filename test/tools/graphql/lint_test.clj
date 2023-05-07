(ns tools.graphql.lint-test
  (:require [clojure.test :refer :all]
            [tools.graphql.lint :as l :refer [lint]]))
(deftest removed-fields
  (let [before
        {:objects {:Notice {:implements [:Node]
                            :fields     {:id          {:type '(non-null ID)}
                                         :title       {:type '(non-null String)}
                                         :description {:type '(non-null String)}}}}}
        after
        {:objects {:Notice {:implements [:Node]
                            :fields     {:id {:type '(non-null ID)}}}}}]

    (is (= 2 (count (:removed-fields (lint before after)))))))


(deftest breaking-changes
  (testing "changing nullable field to non-null is non-breaking"
    (is (not (l/breaking-ret? {:+ '(non-null String), :- 'String})))
    (is (l/breaking-ret? {:- '(non-null String), :+ 'String}))))

#_(deftest changed-types
    (let [non-null
          {:objects {:Notice {:implements [:Node]
                              :fields     {:id    {:type '(non-null ID)}
                                           :title {:type '(non-null String)}}}}}


          nullable
          {:objects {:Notice {:implements [:Node]
                              :fields     {:id    {:type '(non-null ID)}
                                           :title {:type 'String}}}}}]))

(comment
  (run-tests))
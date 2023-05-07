(ns tools.graphql.lint
  (:require [clojure.string :as str]
            [lambdaisland.deep-diff2 :as ddiff]))

(defn keys-in
  "map 에서 가능한 모든 key paths 를 구하여 벡터로 반환"
  [m]
  (letfn [(children [node]
            (let [v (get-in m node)]
              (if (map? v)
                (map (fn [x] (conj node x)) (keys v))
                [])))
          (branch? [node] (-> (children node) seq boolean))]
    (->> (keys m)
         (map vector)
         (mapcat #(tree-seq branch? children %)))))

(defn removed-fields
  "필드가 있다가 없어졌는지 확인"
  [d]
  ;; [... :fields Deletion] 였던 키 확인
  (let [deletion? #(and (record? %)
                        (str/ends-with? (str (type %)) ".Deletion"))]
    (->> (keys-in d)
         (filter #(deletion? (peek %))))))


(defn breaking-ret?
  "반환값은 nullable -> non-nullable 만 non-breaking"
  [{:keys [- +]}]
  (not= (cond
          (list? -)
          (cons 'non-null -)

          :else
          (cons 'non-null (list -)))
        +))

(defn changed-types [d]
  (let [mismatch? #(and (record? %)
                        (str/ends-with? (str (type %)) ".Mismatch"))
        breaking-mismatch? #(and (mismatch? %) (breaking-ret? %))]
    (->> (keys-in d)
         (filter #(= :type (peek %)))
         (filter #(breaking-mismatch? (get-in d %))))))

(defn lint
  "given two args before and after, Lacinia schema conforming maps, find breaking changes between them."
  [before after]
  (let [d (-> (ddiff/diff before after)
              (ddiff/minimize))

        bc (assoc {}
                  :removed-fields (removed-fields d)
                  :changed-types (changed-types d))]

    (cond-> bc)

    bc))

(defn print-result [l]
  (when (seq (:removed-fields l))
    (println "BREAKING CHANGE DETECTED: Some fields are removed.")
    (prn (:removed-fields l)))

  (when (seq (:changed-types l))
    (println "BREAKING CHANGE DETECTED: field types are changed.")
    (prn (:changed-types l))))


(comment
  (let [before
        {:objects {:Notice {:implements [:Node]
                            :fields     {:id    {:type '(non-null ID)}
                                         :title {:type '(non-null String)}}}}}

        after
        {:objects {:Notice {:implements [:Node]
                            :fields     {:id    {:type '(non-null ID)}
                                         :title {:type 'String}}}}}]

    (print-result (lint before after))

    #_(-> (ddiff/diff after before)
          (ddiff/minimize)
          (changed-types))))

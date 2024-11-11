(ns tools.graphql.validators
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [com.stuartsierra.dependency :as dep]
            [meander.epsilon :as m]))

(defn graphql-type?
  "Can we match :User with (list (non-null User)) ?

  formal grammar:
    <type> ::= <named_type>
             | <list_type>
             | <non_null_type>
    <list_type> ::= \"[\" <type> \"]\"
    <non_null_type> ::= <type> \"!\"
    <named_type> ::= <type_name>
    <type_name> ::= <name>
    <name> ::= [_A-Za-z][_0-9A-Za-z]*"
  [t]
  (some? (m/find t
                 (m/with [%list-type     (list %type)
                          %non-null-type (non-null %type)
                          %named-type    (m/keyword !type-name)
                          %type          (m/or %named-type %list-type %non-null-type)]
                         %type)
                 !type-name)))

(defn unreachable-types [schema]
  (let [types        (->> (m/search schema
                                    {(m/or :objects :enums :unions) {?type {:loc ?loc
                                                                            :linters ?opts}}}
                                    [?type ?loc ?opts])
                          ;; `Query` and `Mutation` are always reachable
                          (remove #(-> % first #{:Query :Mutation}))
                          (sort))
        unreachable? (fn [[t _ opts]]
                       (and (not (:ignore opts))
                            (nil? (m/find {:schema schema
                                           :type   t}
                                          ;; [field type] objects, input-objects and interfaces
                                          {:schema {(m/or :objects :input-objects :interfaces) {?type {:fields {?field {:type (m/$ ?t)}}}}}
                                           :type   ?t}
                                          ?field

                                          ;; [argument type] objects and interfaces
                                          {:schema {:objects {?tn {:fields {?fn {:args {?an {:type (m/$ ?t)}}}}}}}
                                           :type   ?t}
                                          ?an

                                          ;; [field type] queries and mutations
                                          {:schema {:objects {(m/or :Query :Mutation) {:fields {?qm {:type (m/$ ?t)}}}}}
                                           :type   ?t}
                                          ?qm

                                          ;; [argument type] queries and mutations
                                          {:schema {:objects {(m/or :Query :Mutation) {:fields {?qm {:args {?input {:type (m/$ ?t)}}}}}}}
                                           :type   ?t}
                                          ?qm

                                          ;; unions
                                          {:schema {:unions {?union {:members (m/$ ?t)}}}
                                           :type   ?t}
                                          ?union))))]
    (filter unreachable? types)))

(defn unreachable-input-types [schema]
  (let [types        (sort (m/search schema
                                     {:input-objects {?type {:loc ?loc}}}
                                     [?type ?loc]))
        unreachable? (fn [[t]]
                       (nil? (m/find {:schema schema
                                      :type   t}
                                     {:schema {:objects {(m/or :Query :Mutation) {:fields {?query {:args {?input {:type (m/$ ?t)}}}}}}}
                                      :type   ?t}
                                     ?query
                                     {:schema {:input-objects {?type {:fields {?field {:type (m/$ ?t)}}}}}
                                      :type   ?t}
                                     ?type)))]
    (log/debug "input-types:" types)
    (filter unreachable? types)))

(defn unreachable-interfaces [schema]
  (let [ifcs         (sort (m/search schema
                                     {:interfaces {?ifc {:loc ?loc}}}
                                     [?ifc ?loc]))
        unreachable? (fn [[ifc]]
                       (nil? (m/find {:schema    schema
                                      :interface ifc}
                                     {:schema    {(m/or :objects :interfaces) {?type {:implements (m/$ ?ifc)}}}
                                      :interface ?ifc}
                                     ?type)))]
    (log/debug "interfaces:" ifcs)
    (filter unreachable? ifcs)))

(defn no-root-resolver [schema]
  (m/search schema
            {:objects {(m/or :Query :Mutation) {:fields {?qm {:resolve (m/and nil ?resolver)}}}}}
            ?qm))

(defn- connection-query? [schema]
  (let [queries     (m/search schema
                              {:queries {?query {:type ?type
                                                 :args ?args}}}
                              {:query ?query, :type ?type, :args ?args})
        connection? (fn [{:keys [type]}]
                      (when-let [type (m/match type
                                               (non-null (m/pred keyword ?type)) ?type
                                               (m/pred keyword? ?type) ?type
                                               _ nil)]
                        (str/ends-with? (name type) "Connection")))]
    (->> queries
         (filter connection?))))

(defn pagination-direction
  "Determine the direction of pagination based on the argument declaration.
  Returns :forward, :backward, :bidirectional or nil (when no matching spec)"
  [args]
  (m/match args
           {:first  {:type 'Int}
            :after  {:type 'String}
            :last   {:type 'Int}
            :before {:type 'String}}
           #_=> :bidirectional

           {:first {:type (m/or Int '(non-null Int))}
            :after {:type 'String}}
           #_=> :forward

           {:last   {:type (m/or Int '(non-null Int))}
            :before {:type 'String}}
           #_=> :backward

           _ nil))

(defn guess-connection-direction [args]
  (m/match args
           {:first  (some? ?first)
            :after  (some? ?after)
            :last   (some? ?last)
            :before (some? ?before)}
           #_=> [:bidirectional ?first ?after ?last ?before]

           {:first {:type ?first}
            :after {:type ?after}}
           #_=> [:forward ?first ?after]

           {:last   {:type ?last}
            :before {:type ?before}}
           #_=> [:backward ?last ?before]

           _ nil))

(defn relay-arguments [schema]
  (->> (connection-query? schema)
       (remove (fn [{:keys [args]}] (pagination-direction args)))
       (map (fn [{:keys [args] :as m}]
              (let [[dir] (guess-connection-direction args)
                    hint (case dir
                           :forward "Forward pagination arguments must be {:first Int, :after String}"
                           :backward "Backward pagination arguments must be {:last Int, :before String}"
                           :bidirectional "Bidirectional pagination arguments must be {:first Int, :after String, :last Int, :before String}"
                           "A field that returns a Connection Type must include forward pagination arguments, backward pagination arguments, or both.")]
                (assoc m :hint hint))))))

(defn interface-with-resolver
  [schema]
  (sort (m/search schema
                  {:interfaces {?ifc {:fields {?field {:resolve (m/some ?resolver)
                                                       :loc     ?loc}}}}}
                  [?ifc ?loc ?field ?resolver])))

(defn- update-deps [g node parents]
  (reduce #(dep/depend %1 node %2) g parents))

(defn- if-dag [schema]
  (let [ifcs  (m/search schema {:interfaces {?ifc {:implements ?parents}}}
                        [?ifc ?parents])
        objs  (m/search schema {:objects {?obj {:implements ?parents}}}
                        [?obj ?parents])
        edges (into (into {} ifcs) objs)]
    (reduce (fn [g [node parents]]
              (update-deps g node parents)) (dep/graph) edges)))

(defn omitted-interfaces [schema]
  (let [dag          (if-dag schema)
        objs         (m/search schema {:objects {?obj {:implements ?parents
                                                       :loc        ?loc}}}
                               [?obj ?loc ?parents])
        obj-loc      (zipmap (map first objs) (map second objs))
        obj-deps     (zipmap (map first objs) (map last objs))
        omitted-diff (fn [obj]
                       (let [actual-deps    (dep/transitive-dependencies dag obj)
                             described-deps (set (obj-deps obj))]
                         (set/difference actual-deps described-deps)))]
    (->> (keys obj-deps)
         (map (juxt identity obj-loc omitted-diff))
         (filter (fn [[_ _ omitted]] (seq omitted))))))

(comment

  (require '[clojure.java.io :as io])
  @(def schema (read-string (slurp (io/resource "unreachable.edn"))))

  (unreachable-types schema)
  (unreachable-input-types schema)
  (unreachable-interfaces schema)
  (interface-with-resolver schema)

  (def schema (read-edn (io/resource "nested-interfaces.edn")))
  (omitted-interfaces schema)

  (no-root-resolver schema)
  (relay-arguments schema)

  (def schema (read-edn (io/resource "pagination.edn")))
  (guess-connection-direction {:first          {:type '(non-null Int)},
                               :after          {:type 'ID},
                               :orderBy        {:type :CommunityPostCommentOrderBy, :default-value :CREATED_AT},
                               :orderDirection {:type :OrderDirection, :default-value :DESC}})

  (update-deps (dep/graph) :Post [:Node])

  :rcf)

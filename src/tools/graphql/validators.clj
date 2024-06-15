(ns tools.graphql.validators
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
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
  (let [types        (sort (m/search schema
                                     {(m/or :objects :enums :unions) {?type {:loc ?loc}}}
                                     [?type ?loc]))
        unreachable? (fn [[t]]
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
                                     {:schema {(m/or :queries :mutations) {?qm {:type (m/$ ?t)}}}
                                      :type   ?t}
                                     ?qm

                                     ;; [argument type] queries and mutations
                                     {:schema {(m/or :queries :mutations) {?qm {:args {?input {:type (m/$ ?t)}}}}}
                                      :type   ?t}
                                     ?qm

                                     ;; unions
                                     {:schema {:unions {?union {:members (m/$ ?t)}}}
                                      :type   ?t}
                                     ?union)))]
    (log/debug "types:" types)
    (filter unreachable? types)))

(defn unreachable-input-types [schema]
  (let [types        (sort (m/search schema
                                     {:input-objects {?type {:loc ?loc}}}
                                     [?type ?loc]))
        unreachable? (fn [[t]]
                       (nil? (m/find {:schema schema
                                      :type   t}
                                     {:schema {(m/or :queries :mutations) {?query {:args {?input {:type (m/$ ?t)}}}}}
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
            {(m/or :queries :mutations) {?qm {:resolve (m/and nil ?resolver)}}}
            ?qm))

(defn connection-query? [schema]
  (let [queries     (m/search schema
                              {:queries {?query {:type ?type}}}
                              [?query ?type])
        connection? (fn [[_ type]]
                      (when-let [type (m/match type
                                               (non-null (m/pred keyword ?type)) ?type
                                               (m/pred keyword? ?type) ?type
                                               _ nil)]
                        (str/ends-with? (name type) "Connection")))]
    (->> queries
         (filter connection?)
         #_(map first))))

(connection-query? schema)

(comment

  (require '[tools.graphql.stitch.core :refer [read-edn]]
           '[clojure.java.io :as io])
  (def schema (read-edn (io/file "../farmmorning-backend/bases/core-api/resources/superschema.edn")))
  (def schema (read-edn (io/file (io/resource "unreachable.edn"))))

  (->> (unreachable-types schema)
       (remove #(str/ends-with? (name %) "Connection"))
       #_(count))

  (unreachable-types schema)
  (unreachable-input-types schema)
  (unreachable-interfaces schema)

  (no-root-resolver schema)


  (def schema
    (read-edn (io/resource "pagination.edn")))

  ;; find queries retuning Connection type
  (connection-query? schema)

  (m/match '(non-null :PostConnection)
           (non-null ?type) ?type
           (m/keyword ?type) ?type)
  (m/match :PostConnection
           (non-null ?type) ?type
           (m/pred keyword? ?type) ?type)
  (m/match '(non-null (list :PostConnection))
           (non-null (m/pred keyword ?type)) ?type
           (m/pred keyword? ?type) ?type
           _ nil)

  :rcf)

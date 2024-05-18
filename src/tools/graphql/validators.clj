(ns tools.graphql.validators
  (:require [meander.epsilon :as m]))

(defn- graphql-type?
  "
  <type> ::= <named_type>
           | <list_type>
           | <non_null_type>

  <list_type> ::= \"[\" <type> \"]\"

  <non_null_type> ::= <type> \"!\"

  <named_type> ::= <type_name>

  <type_name> ::= <name>

  <name> ::= [_A-Za-z][_0-9A-Za-z]*
  "
  [t]
  (m/find t
          (m/with [%list-type     (list %type)
                   %non-null-type (non-null %type)
                   %named-type    (m/keyword !type-name)
                   %type          (m/or %named-type %list-type %non-null-type)]
            %type)
          !type-name))

(defn unreachable-types [schema]
  (let [types        (m/search schema
                               {:objects {?type _}}
                               ?type)
        unreachable? (fn [t]
                       (nil? (m/find {:schema schema
                                      :type   t}
                                     {:schema {:objects {?type {:fields {?field {:type (m/$ ?t)}}}}}
                                      :type   ?t}
                                     ?field
                                     ;; TODO: input-objects, enums, interfaces, unions
                                     {:schema {(m/or :queries :mutations) {?qm {:type (m/$ ?t)}}}
                                      :type   ?t}
                                     ?qm)))]
    (println "types:" (sort types))
    (filter unreachable? types)))

(defn unreachable-input-types [schema]
  (let [types        (m/search schema
                               {:input-objects {?type _}}
                               ?type)
        unreachable? (fn [t]
                       (nil? (m/find {:schema schema
                                      :type   t}
                                     {:schema {(m/or :queries :mutations) {?query {:args {?input {:type (m/$ ?t)}}}}}
                                      :type   ?t}
                                     ?query
                                     {:schema {:input-objects {?type {:fields {?field {:type (m/$ ?t)}}}}}
                                      :type   ?t}
                                     ?type)))]
    (println "input-types:" (sort types))
    (filter unreachable? types)))

(comment

  (require '[tools.graphql.stitch.impl :refer [read-edn]]
           '[clojure.java.io :as io])
  (def schema (read-edn (io/file "../../bases/core-api/resources/superschema.edn")))
  (def schema (read-edn (io/file (io/resource "unreachable.edn"))))

  (unreachable-types schema)
  (unreachable-input-types schema)

  ;; can we match :User with (list (non-null User)) ?

  (let [ts [:User
            '(list :User)
            '(non-null :User)
            '(list (non-null :User))
            '(non-null (list :User))
            '(non-null (list (list :User)))
            '(:User list)]]
    (map graphql-type? ts)))

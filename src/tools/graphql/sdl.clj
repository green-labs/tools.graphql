(ns tools.graphql.sdl
  (:require [clojure.string :as s]))

(def ^:private convert-keys
  #{:enums :interfaces :objects :queries :mutations :unions :input-objects :scalars})

(def ^:private primitive-types
  #{'String 'Int 'Float 'Boolean 'ID})

(defmulti ->sdl
  "lacinia edn schema 를 graphql sdl 로 변환합니다."
  (fn [[k _]]
    (convert-keys k)))

(defn edn->sdl
  "lacinia edn schema 를 graphql sdl 로 변환합니다."
  [m]
  (mapcat ->sdl (vec m)))

(defn ->doc
  ([doc] (->doc doc ""))
  ([doc indent]
   (when doc
     (s/join [indent "\"\"\"\n"
              indent doc "\n"
              indent "\"\"\"\n"]))))

(defn ->type
  [expr]
  (if (seq? expr)
    (let [[s1 s2] expr]
      (if s2
        (case s1
          non-null (str (->type s2) "!")
          list (str "[" (->type s2) "]"))
        (name s1)))
    (name expr)))

(defn ->value
  [expr]
  (cond
    (keyword? expr)
    (name expr)

    (string? expr)
    (str "\"" expr "\"")

    (sequential? expr)
    (str "[" (s/join ", " (map ->value expr)) "]")

    :else
    (str expr)))

(declare ->arg)

(defn- ->directive [directive args]
  (let [args' (->> args
                   (map (fn [[k v]]
                          (let [k' (name k)
                                v' (if (string? v) (str \" v \") v)]
                            (str k' ": " v'))))
                   (s/join ", "))]
    (str "@" directive (when args (str "(" args' ")")))))

(defn- ->deprecated [deprecated]
  (->directive "deprecated" (when deprecated {:reason deprecated})))

(defn ->field
  ([m] (->field m ""))
  ([[field-name {:keys [description type args default-value deprecated]}] indent]
   (str (->doc description indent)
        indent (name field-name)
        (when args (->> (->arg args)
                        (s/split-lines)
                        (s/join (str "\n" indent))))
        ": "
        (->type type)
        (when default-value
          (str " = " (->value default-value)))
        (when deprecated (str " " (->deprecated deprecated))))))

(defn ->arg
  ([m] (->arg m ""))
  ([m indent]
   (when (seq m)
     (str "(\n"
          (->> m
               vec
               (map #(->field % (str indent "  ")))
               (s/join "\n"))
          "\n"
          indent
          ")"))))

(defmethod ->sdl :enums
  [[_ m]]
  (map (fn [[k {:keys [description values]}]]
         (str (->doc description)
              "enum " (name k) " {\n"
              (->> values
                   (map (fn [{:keys [description deprecated enum-value]}]
                          (str
                           (->doc description "  ")
                           "  "
                           (name enum-value)
                           (when deprecated (str " " (->deprecated deprecated))))))
                   (s/join "\n"))
              "\n}\n"))
       (vec m)))

(defn- object&input&interface->sdl
  [type m]
  (map (fn [[k {:keys [description implements fields]}]]
         (str (->doc description)
              type
              " " (name k)
              (when implements
                (str " implements "
                     (->> implements
                          (map name)
                          (s/join " & "))))
              " {\n"
              (->> fields
                   vec
                   (map #(->field % "  "))
                   (s/join "\n"))
              "\n}\n"))
       (vec m)))

(defmethod ->sdl :interfaces
  [[_ m]]
  (object&input&interface->sdl "interface" m))

(defmethod ->sdl :objects
  [[_ m]]
  (object&input&interface->sdl "type" m))

(defmethod ->sdl :input-objects
  [[_ m]]
  (object&input&interface->sdl "input" m))

(defn- query&mutation->sdl
  [type m]
  (str "type "
       type
       " {\n"
       (->> m
            (map (fn [[k {:keys [deprecated description args type]}]]
                   (str (->doc description "  ")
                        "  "
                        (name k)
                        (->arg args "  ")
                        ": "
                        (->type type)
                        (when deprecated (str " " (->deprecated deprecated))))))
            (s/join "\n"))
       "\n}\n"))

(defmethod ->sdl :queries
  [[_ m]]
  [(query&mutation->sdl "Query" m)])

(defmethod ->sdl :mutations
  [[_ m]]
  [(query&mutation->sdl "Mutation" m)])

(defmethod ->sdl :unions
  [[_ m]]
  (map (fn [[k {:keys [description members]}]]
         (str (->doc description)
              "union " (name k) " = \n "
              (->> members
                   (map #(str " " (name %)))
                   (s/join "\n|"))
              "\n"))
       (vec m)))

(defmethod ->sdl :scalars
  [[_ m]]
  (map (fn [[k {:keys [description]}]]
         (str (->doc description)
              "scalar " (name k) "\n"))
       (vec m)))

(defmethod ->sdl :default
  [_]
  nil)

(defn inner-type
  [expr]
  (if (seq? expr)
    (let [[s1 s2] expr]
      (if s2
        (inner-type s2)
        s1))
    expr))

(defn get-object
  [schema type-name]
  (or (get-in schema [:objects (keyword type-name)])
      (get-in schema [:objects (symbol type-name)])))

(defn- duplicate-type?
  [schema field memo-set]
  (let [objects    (set (keys (:objects schema)))
        field-type (inner-type (:type (second field)))]
    (and (contains? objects field-type)
         (contains? memo-set field))))

(defn ->args
  ([args]
   (->args args nil))
  ([args prefix]
   (str "(\n"
        (->> (keys args)
             (map #(let [k' (name %)]
                     (str k' ": $" prefix k')))
             (s/join "\n"))
        "\n)")))

(defn ->field-names
  ([schema object-def]
   (->field-names schema object-def #{}))
  ([schema object-def memo-set]
   (->> (get object-def :fields {})
        vec
        (map (fn [[k {:keys [type args]} :as parent]]
               (when-not (duplicate-type? schema parent memo-set)
                 (str (name k)
                      (when args (->args args (name k)))
                      (when-let [object-def (get-object schema (inner-type type))]
                        (str " {\n"
                             (->field-names schema object-def (conj memo-set parent))
                             "\n}"))))))
        (s/join "\n"))))

(defn args->query-args
  ([args]
   (args->query-args args nil))
  ([args prefix]
   (map (fn [[k v]]
          [(str "$" prefix (name k))
           (select-keys v [:type :default-value])])
        args)))

(defn extract-field-args
  ([schema type]
   (extract-field-args schema type #{}))
  ([schema type memo-set]
   (let [return-only-type (inner-type type)]
     (if-let [union (get-in schema [:unions return-only-type :members])]
       (mapcat (fn [type]
                 (extract-field-args schema type memo-set))
               union)
       (->> (get (get-object schema return-only-type) :fields {})
            vec
            (mapcat (fn [[field-name {:keys [type args]} :as parent]]
                      (when-not (duplicate-type? schema parent memo-set)
                        (concat
                         (when (get-object schema (inner-type type))
                           (extract-field-args schema type (conj memo-set parent)))
                         (when args
                           (args->query-args args (name field-name))))))))))))

(defn- query&mutation->query
  [schema query-type query-name {:keys [args type]}]
  (let [return-only-type (inner-type type)
        primitive-type?  (primitive-types return-only-type)
        query-args       (args->query-args args)
        field-args       (extract-field-args schema return-only-type)]
    (str query-type " " (name query-name)
         (->arg (->> (concat query-args field-args)
                     (into {})))
         " {\n"
         (name query-name)
         (when (seq args)
           (->args args))
         (when-not primitive-type?
           "{\n")
         (if-let [union (get-in schema [:unions return-only-type :members])]
           (->> union
                (map (fn [type]
                       (str "... on " (name type) " {\n"
                            (->field-names schema (get-object schema type))
                            "\n}")))
                (s/join "\n"))
           (->field-names schema (get-object schema return-only-type)))
         (when-not primitive-type?
           "\n}")
         "\n}")))

(defn ->query
  "lacinia edn 에 있는 query 중 query-name 을 찾아 GraphQL에 요청 할 수 있는 query를 만듭니다.
  input
  - schema: lacinia edn
  - query-name: edn 에 지정된 query 이름
  output
  - GraphQL query string"
  [schema query-name]
  (when-let [query-def (get-in schema [:queries query-name])]
    (query&mutation->query schema "query" query-name query-def)))

(defn ->mutation
  "lacinia edn 에 있는 mutation 중 mutation-name 을 찾아 GraphQL에 요청 할 수 있는 mutation을 만듭니다.
  input
  - schema: lacinia edn
  - mutation-name: edn 에 지정된 mutation 이름
  output
  - GraphQL mutation string"
  [schema mutation-name]
  (when-let [mutation-def (get-in schema [:mutations mutation-name])]
    (query&mutation->query schema "mutation" mutation-name mutation-def)))

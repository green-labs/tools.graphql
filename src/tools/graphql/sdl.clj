(ns tools.graphql.sdl
  (:require [clojure.string :as s]))

(def ^:private convert-keys
  #{:enums :interfaces :objects :queries :mutations :unions :input-objects :scalars})

(defmulti ->sdl
  (fn [[k _]]
    (convert-keys k)))

(defn edn->sdl
  "Convert Lacinia schema edn to GraphQL SDL string."
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
        (when (seq args)
          (->> (->arg args)
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

(defn ->args
  ([args]
   (->args args ""))
  ([args prefix]
   (str "(\n"
        (->> (keys args)
             (map #(let [k' (name %)]
                     (str k' ": $" (name prefix) k')))
             (s/join "\n"))
        "\n)")))

(defmethod ->sdl :enums
  [[_ m]]
  (map (fn [[k {:keys [description values]}]]
         (str (->doc description)
              "enum " (name k) " {\n"
              (->> values
                   (map (fn [{:keys [description deprecated enum-value] :as value}]
                          (if (keyword? value)
                            (str "  " (name value))
                            (str
                             (->doc description "  ")
                             "  "
                             (name enum-value)
                             (when deprecated (str " " (->deprecated deprecated)))))))
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

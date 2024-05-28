
# tools.graphql

This library provides various tools related to GraphQL.

1. [Convert EDN Schema to SDL](#convert-edn-schema-to-sdl)
2. [Merge EDN Schemas](#merge-edn-schemas)
3. [Schema Static Analysis](#schema-static-analysis)

## Installation

You can install this library using the Clojure CLI tool.

```sh
clojure -Ttools install-latest :lib io.github.green-labs/tools.graphql :as graphql
```

If you want to add it to your build system, add the following to the `:build` (or appropriate alias):

```edn
{io.github.clojure/tools.build {:mvn/version "0.10.3"}}
```

### Convert EDN Schema to SDL

When using the Lacinia library, you need to write the schema in EDN format. If you want to convert it to GraphQL SDL format, you can use the `edn2sdl` API.

```sh
clojure -Tgraphql edn2sdl \
  :input-path '"path/to/schema.edn"' \
  :output-path '"path/to/schema.graphql"'
```

### Merge EDN Schemas

As your service grows, it may be convenient to write the EDN schema in multiple files. However, Lacinia can only read a single schema.

Although it is possible to merge and load these at runtime, this approach is not recommended. It can be difficult to ensure that there are no conflicts between the schemas or that all necessary fields are defined.

Therefore, you might want to have a separate build step to merge the schemas. You can use the `stitch-schemas` API for this purpose.

```sh
clojure -Tgraphql stitch-schemas \
  :input-paths '["path/to/schema1/" "path/to/schema2/"]' \
  :output-path '"path/to/schema.edn"'
```

The `stitch-schemas` function will only generate the final schema if it can merge the schemas reliably.

If you frequently need to merge schemas during development, you can add the `:watch true` parameter to enable the watch feature. To use watch, Watchman must be installed on your system.

Note: It is recommended to commit the stitched schema alongside with subschemas.

### Schema Static Analysis

To maintain consistency and avoid using incorrect fields or types when writing your schema, you might need a tool to assist you, especially as the schema grows larger and more complex. For this, you can use the `validate` API.

```sh
clojure -Tgraphql validate :input-path '"path/to/schema.edn"'
```

This command will identify unused `type`, `input`, `enum`, `union`, and `interface`.

Example:

```text
Unreachable type UrlConnection bases/core-api/resources/schema/common.edn:62:30
Unreachable type UserAddressConnection
Unreachable type YoutubeConnection
Unreachable input GroupMembershipUpdateInput bases/core-api/resources/schema/farming_group.edn:238:3
Unreachable input ProductionCategoryCodeInput bases/core-api/resources/schema/pesticide.edn:141:47
Unreachable interface Node bases/core-api/resources/schema/common.edn:6:1
```

To output the location in the subschema, you must set `:source-map?` option to true when stitching.
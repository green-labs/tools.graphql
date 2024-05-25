~~# tools.graphql

이 라이브러리는 graphql 관련된 여러가지 도구들을 제공한다.

1. [edn 스키마를 sdl 로 변환하기](#edn-스키마를-sdl-로-변환하기)
2. [edn 스키마 합치기](#스키마-edn-합치기)
3. [스키마 정적 분석](#스키마-정적-분석)


## 설치

이 라이브러리는 clojure cli tool 로 설치할 수 있다.

```sh
clojure -Ttools install-lastest :lib io.github.green-labs/tools.graphql :as graphql
```

빌드 시스템에 추가하고자 할 경우 :build (혹은 적절한 alias) 에 다음을 추가하시오.

```edn
{io.github.clojure/tools.build {:mvn/version "0.10.3"}}
```


### edn 스키마를 sdl 로 변환하기

lacinia 라이브러리를 사용하는 경우 edn 형식으로 스키마를 작성해야 한다.
만약 이를 GraphQL SDL 형태로 변환하고 싶을 경우 edn2sdl API 를 사용할 수 있다.

```sh
clojure -Tgraphql edn2sdl \
  :input-path "path/to/schema.edn" \
  :output-path "path/to/schema.graphql"
```


### edn 스키마 합치기

서비스가 커지면 edn 스키마를 여러개로 나누어 작성하는 것이 편리하다.
그러나 Lacinia 는 하나의 스키마만을 읽을 수 있다.

런타임에 이를 합쳐서 로딩하는 방법도 있지는 이를 추천하지 않는다.
왜냐하면 각각의 스키마가 서로 충돌하지 않는지, 혹은 필요한 필드가 모두 정의되어 있는지 확인하기 어렵기 때문이다.

따라서 별도의 빌드 단계를 두어 스키마를 합치는 것을 원할 수 있다.
이를 위해 stitch-schemas API 를 사용할 수 있다.

```sh
clojure -Tgraphql stitch-schemas \
  :input-paths ["path/to/schema1/" "path/to/schema2/"] \
  :output-path "path/to/schema.edn"
```

stitch-schema 는 안정적으로 스키마를 합칠 수 있는 경우에만 최종 스키마를 생성한다.

개발 단계에서 스키마 합치기를 자주 사용한다면 `:watch true` 인자를 추가하여 watch 기능을 사용할 수 있다.
watch 를 사용하기 위해서는 watchman 이 시스템에 설치되어 있어야 한다.

Note: 최종 생성된 스키마 역시 버전 관리를 하는 것이 좋다.


### 스키마 정적 분석

스키마를 작성할 때 일관성을 유지하고, 잘못된 필드나 타입을 사용하지 않도록 도와주는 도구가 필요할 수 있다.
특히 스키마가 크고 복잡해질 경우 이는 더욱 중요하다.
이를 위해 validate API 를 사용할 수 있다.

```sh
clojure -Tgraphql validate :input-path "path/to/schema.edn"
```

위 명령어는 사용하지 않는 type, input, enum, union, interface 를 찾아줄 것이다.~~

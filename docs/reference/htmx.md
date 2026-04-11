---
id: htmx
title: "HTMX"
---

`zio-blocks-htmx` adds a typed HTMX DSL on top of [Template](./template.md) and reuses [HTTP Model](./http-model.md) for typed request and response headers.

## Overview

The module focuses on two pieces of the HTMX workflow:

- Typed HTML attributes such as `hxGet`, `hxTarget`, `hxSwap`, `hxTrigger`, `hxVals`, and `hxHeaders`
- Typed HTTP headers for reading HTMX requests and emitting HTMX responses

This keeps HTML rendering, JSON payload rendering, and HTTP header handling in one place:

```text
Template DOM + HTMX attributes         HTTP request/response headers
             |                                      |
             v                                      v
      rendered HTML                         typed Header values
             |                                      |
             +------------ server-driven UI --------+
```

## Installation

Add the module to your `build.sbt`:

```scala
libraryDependencies += "dev.zio" %% "zio-blocks-htmx" % "@VERSION@"
```

For Scala.js projects, use the cross-platform artifact:

```scala
libraryDependencies += "dev.zio" %%% "zio-blocks-htmx" % "@VERSION@"
```

The module depends on `zio-blocks-template` and `zio-http-model`, so you do not need to add them separately unless you want to depend on them directly.

## Rendering HTMX Attributes

We use the normal Template element DSL and add typed HTMX attributes with the same `:=` style:

```scala mdoc:compile-only
import java.time.Duration

import zio.blocks.htmx._
import zio.blocks.template._

val search = form(
  hxPost := "/search",
  hxTarget := HxTarget.find("#results"),
  hxSwap := HxSwap.InnerHTML.settle(Duration.ofMillis(200)),
  hxTrigger := HxTrigger.submit,
  hxIndicator := CssSelector.id("spinner")
)(
  input(
    `type` := "text",
    new PartialAttribute("name") := "q",
    hxGet := "/suggest",
    hxTrigger := HxTrigger.input.changed.delay(Duration.ofMillis(300)),
    hxTarget := HxTarget.next(".suggestions")
  ),
  button(`type` := "submit")("Search")
)
```

The rendered HTML is still plain HTMX markup, but the intermediate Scala values are typed.

## Targets, Swaps, and Triggers

The module models the HTMX string mini-languages as small ADTs instead of raw strings.

We can target extended selectors with `HxTarget`:

```scala mdoc:compile-only
import zio.blocks.htmx._

val closestForm = HxTarget.closest("form")
val nextResults = HxTarget.next(".results")
val spinner     = HxTarget.css("#spinner")
```

We can build swap behavior with `HxSwap`:

```scala mdoc:compile-only
import java.time.Duration

import zio.blocks.htmx._

val swap =
  HxSwap.InnerHTML
    .swap(Duration.ofSeconds(1))
    .settle(Duration.ofMillis(250))
    .show(HxSwap.ShowPosition.top)
```

We can build event triggers with `HxTrigger`:

```scala mdoc:compile-only
import java.time.Duration

import zio.blocks.htmx._

val trigger =
  HxTrigger.click
    .changed
    .delay(Duration.ofMillis(300))
    .queue(HxTrigger.QueueStrategy.Last)
```

## Event Handlers

We can attach inline HTMX event handlers with `hxOn` without generating one definition per DOM event:

```scala mdoc:compile-only
import zio.blocks.htmx._
import zio.blocks.template._

val buttonDom = button(
  hxOn.click := js"console.log('clicked')",
  hxOn("custom-event") := Js("handleCustom(event)")
)("Save")
```

## JSON Attributes

`hxVals` and `hxHeaders` support both static JSON and dynamic `js:` forms.

We can render schema-backed JSON payloads with `HxVals.from` and `HxHeaders.from`:

```scala mdoc:compile-only
import zio.blocks.htmx._
import zio.blocks.schema.Schema
import zio.blocks.template._

final case class SearchParams(term: String, page: Int)
object SearchParams {
  implicit val schema: Schema[SearchParams] = Schema.derived
}

val inputDom = input(
  hxVals := HxVals.from(SearchParams("zio", 2)),
  hxHeaders := HxHeaders.from(Map("X-Requested-With" -> "htmx"))
)
```

We can also render dynamic HTMX expressions with `HxVals.js` and `HxHeaders.js`:

```scala mdoc:compile-only
import zio.blocks.htmx._
import zio.blocks.template._

val dynamicDom = div(
  hxVals := HxVals.js(Js("{ page: currentPage(), q: event.target.value }")),
  hxHeaders := HxHeaders.js(Js("{ Authorization: token() }"))
)
```

## Typed Response Headers

The `zio.blocks.htmx.headers` package exposes `Header.Typed` instances that plug directly into `Headers`.

We can emit typed HTMX response headers like this:

```scala mdoc:compile-only
import zio.blocks.htmx.HxSwap
import zio.blocks.htmx.headers._
import zio.http._

val location = HXLocation(
  path = URL.fromPath(Path.root / "items") ?? ("page", "2"),
  target = Some("#results"),
  swap = Some(HxSwap.OuterHTML)
)

val responseHeaders = Headers.empty
  .add(HXLocation.name, HXLocation.render(location))
  .add(HXTrigger.name, HXTrigger.render(HXTrigger(HXTriggerValue.names("refresh-list"))))
  .add(HXPushUrl.name, HXPushUrl.render(HXPushUrl(HXUrlValue.url(URL.fromPath(Path.root / "items")))))
```

`HXTrigger`, `HXTriggerAfterSettle`, and `HXTriggerAfterSwap` support both simple event names and structured JSON payloads.

## Typed Request Headers

We can parse HTMX request headers through the same `Headers#get` API used elsewhere in `http-model`:

```scala mdoc:compile-only
import zio.blocks.htmx.headers._
import zio.http.Headers

val headers = Headers(
  HXRequest.name -> "true",
  HXCurrentURL.name -> "/items?page=2",
  HXTarget.name -> "results"
)

val isHtmx      = headers.get(HXRequest)
val currentUrl  = headers.get(HXCurrentURL)
val targetId    = headers.get(HXTarget)
```

The request-side `HX-Trigger` parser is exposed as `HXTriggerId` in Scala to avoid colliding with the response-side `HXTrigger` type.

## Complete Example

We can combine the HTML DSL and the header DSL in the same endpoint or handler:

```scala mdoc:compile-only
import java.time.Duration

import zio.blocks.htmx._
import zio.blocks.htmx.headers._
import zio.blocks.template._
import zio.http._

val body = div(
  hxGet := "/items?page=2",
  hxTrigger := HxTrigger.revealed,
  hxSwap := HxSwap.AfterEnd,
  hxIndicator := CssSelector.`class`("spinner")
)("Loading...")

val headers = Headers(
  HXReswap.name -> HXReswap.render(HXReswap(HxSwap.AfterEnd)),
  HXTriggerAfterSwap.name -> HXTriggerAfterSwap.render(
    HXTriggerAfterSwap(HXTriggerValue.detail("loaded", Map("page" -> 2)))
  )
)

val response = Response.ok.copy(headers = headers, body = Body.fromString(body.render))
```

## Related Modules

`zio-blocks-htmx` builds on these modules:

- [Template](./template.md) for `Dom`, `Js`, `CssSelector`, and modifier conversions
- [HTTP Model](./http-model.md) for `Header`, `Headers`, and `URL`

If you need HTML rendering without HTMX semantics, use [Template](./template.md) directly. If you need HTTP modeling without HTML attributes, use [HTTP Model](./http-model.md) directly.

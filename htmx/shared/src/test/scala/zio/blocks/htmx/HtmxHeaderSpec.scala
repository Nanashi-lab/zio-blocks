package zio.blocks.htmx

import scala.collection.immutable.ListMap

import zio.blocks.htmx.headers._
import zio.http._
import zio.test._

object HtmxHeaderSpec extends ZIOSpecDefault {
  def spec = suite("HtmxHeader")(
    suite("request headers")(
      test("parse typed request headers via Headers.get") {
        val headers = Headers(
          HXRequest.name -> "true",
          HXBoosted.name -> "false",
          HXCurrentURL.name -> "/items?page=2",
          HXTarget.name -> "results",
          HXTriggerId.name -> "search-input",
          HXTriggerName.name -> "q",
          HXHistoryRestoreRequest.name -> "true",
          HXPrompt.name -> "Really?"
        )

        assertTrue(
          headers.get(HXRequest).contains(HXRequest(htmx = true)),
          headers.get(HXBoosted).contains(HXBoosted(boosted = false)),
          headers.get(HXCurrentURL).contains(HXCurrentURL((URL.fromPath(Path.root / "items") ?? ("page", "2")))),
          headers.get(HXTarget).contains(HXTarget("results")),
          headers.get(HXTriggerId).contains(HXTriggerId("search-input")),
          headers.get(HXTriggerName).contains(HXTriggerName("q")),
          headers.get(HXHistoryRestoreRequest).contains(HXHistoryRestoreRequest(restoring = true)),
          headers.get(HXPrompt).contains(HXPrompt("Really?"))
        )
      },
      test("reject invalid boolean and URL request header values") {
        assertTrue(
          HXRequest.parse("yes").isLeft,
          HXBoosted.parse("1").isLeft,
          HXHistoryRestoreRequest.parse("no").isLeft,
          HXCurrentURL.parse("").isLeft
        )
      },
      test("Headers.get skips invalid raw values and returns a later valid typed header") {
        val headers = Headers(
          HXRequest.name -> "not-bool",
          HXRequest.name -> "true"
        )

        assertTrue(headers.get(HXRequest).contains(HXRequest(htmx = true)))
      },
      test("request headers render headerName and renderedValue") {
        val header = HXCurrentURL(URL.fromPath(Path.root / "search") ?? ("q", "zio"))

        assertTrue(
          header.headerName == HXCurrentURL.name,
          header.renderedValue == "/search?q=zio"
        )
      }
    ),
    suite("response headers")(
      test("render and parse url-or-false response headers") {
        val nextPage = URL.fromPath(Path.root / "items") ?? ("page", "2")
        val headers = Headers.empty
          .add(HXPushUrl.name, HXPushUrl.render(HXPushUrl(HXUrlValue.url(nextPage))))
          .set(HXReplaceUrl.name, HXReplaceUrl.render(HXReplaceUrl(HXUrlValue.False)))

        assertTrue(
          headers.get(HXPushUrl).contains(HXPushUrl(HXUrlValue.url(nextPage))),
          headers.get(HXReplaceUrl).contains(HXReplaceUrl(HXUrlValue.False))
        )
      },
      test("reject invalid url-or-false response header values") {
        assertTrue(
          HXPushUrl.parse("http://example.com:abc").isLeft,
          HXReplaceUrl.parse("http://example.com:abc").isLeft,
          HXRedirect.parse("").isLeft
        )
      },
      test("render and parse redirect, refresh, reswap, retarget, and reselect") {
        val redirect = HXRedirect(URL.root.host("example.com").relative / "login")
        val headers = Headers(
          HXRedirect.name -> HXRedirect.render(redirect),
          HXRefresh.name -> HXRefresh.render(HXRefresh()),
          HXReswap.name -> HXReswap.render(HXReswap(HxSwap.AfterEnd)),
          HXRetarget.name -> "#panel",
          HXReselect.name -> ".item"
        )

        assertTrue(
          headers.get(HXRedirect).contains(redirect),
          headers.get(HXRefresh).contains(HXRefresh()),
          headers.get(HXReswap).contains(HXReswap(HxSwap.AfterEnd)),
          headers.get(HXRetarget).contains(HXRetarget("#panel")),
          headers.get(HXReselect).contains(HXReselect(".item"))
        )
      },
      test("reject invalid refresh and reswap values") {
        assertTrue(
          HXRefresh.parse("1").isLeft,
          HXReswap.parse("morph").isLeft
        )
      },
      test("render and parse hx-location JSON payloads") {
        val location = HXLocation(
          path = URL.fromPath(Path.root / "items") ?? ("page", "2"),
          target = Some("#results"),
          swap = Some(HxSwap.OuterHTML),
          select = Some("#results")
        )

        val rendered = HXLocation.render(location)

        assertTrue(
          rendered ==
            """{"path":"/items?page=2","target":"#results","swap":"outerHTML","select":"#results"}""",
          HXLocation.parse(rendered) == Right(location)
        )
      },
      test("parse hx-location plain string form") {
        val location = HXLocation(URL.fromPath(Path.root / "redirect"))

        assertTrue(HXLocation.parse("/redirect") == Right(location))
      },
      test("reject malformed hx-location payloads") {
        assertTrue(
          HXLocation.parse("{").isLeft,
          HXLocation.parse("""{"target":"#results"}""").isLeft,
          HXLocation.parse("""{"path":"/items","swap":"morph"}""").isLeft
        )
      },
      test("render and parse hx-trigger response headers") {
        val triggered = zio.blocks.htmx.headers.HXTrigger(
          HXTriggerValue.targeted(
            "showMessage",
            "#toast",
            "message" -> HXJsonValue.from("Saved"),
            "level" -> HXJsonValue.from("info")
          )
        )
        val rendered = zio.blocks.htmx.headers.HXTrigger.render(triggered)

        assertTrue(
          rendered ==
            """{"showMessage":{"target":"#toast","message":"Saved","level":"info"}}""",
          zio.blocks.htmx.headers.HXTrigger.parse(rendered) == Right(triggered)
        )
      },
      test("parse plain single-name trigger headers") {
        assertTrue(
          zio.blocks.htmx.headers.HXTrigger.parse("refresh") ==
            Right(zio.blocks.htmx.headers.HXTrigger(HXTriggerValue.names("refresh")))
        )
      },
      test("render plain and multi-name trigger headers") {
        val afterSettle = HXTriggerAfterSettle(HXTriggerValue.names("settled", "analytics"))
        val afterSwap   = HXTriggerAfterSwap(HXTriggerValue.detail("refreshed", ListMap("count" -> 2)))

        assertTrue(
          HXTriggerAfterSettle.render(afterSettle) == "settled, analytics",
          HXTriggerAfterSettle.parse("settled, analytics") == Right(afterSettle),
          HXTriggerAfterSwap.render(afterSwap) == """{"refreshed":{"count":2}}""",
          HXTriggerAfterSwap.parse("""{"refreshed":{"count":2}}""") == Right(afterSwap)
        )
      },
      test("reject malformed trigger response header values") {
        assertTrue(
          zio.blocks.htmx.headers.HXTrigger.parse("").isLeft,
          HXTriggerAfterSwap.parse("{").isLeft
        )
      },
      test("response headers expose headerName and renderedValue") {
        val header = HXRetarget("#toast")

        assertTrue(
          header.headerName == HXRetarget.name,
          header.renderedValue == "#toast"
        )
      }
    )
  )
}

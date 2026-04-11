package zio.blocks.htmx

import java.time.Duration

import zio.blocks.template._
import zio.test._

object HtmxIntegrationRenderSpec extends ZIOSpecDefault {
  private val name = new PartialAttribute("name")

  def spec = suite("HtmxIntegrationRender")(
    test("renders the search form example") {
      val searchForm = form(
        hxPost := "/search",
        hxTarget := HxTarget.find("#results"),
        hxSwap := HxSwap.InnerHTML.settle(Duration.ofMillis(200)),
        hxTrigger := HxTrigger.submit,
        hxIndicator := CssSelector.id("spinner")
      )(
        input(
          typeAttr := "text",
          name := "q",
          hxGet := "/suggest",
          hxTrigger := HxTrigger.input.changed.delay(Duration.ofMillis(300)),
          hxTarget := HxTarget.next(".suggestions")
        ),
        button(typeAttr := "submit")("Search")
      )

      assertTrue(
        searchForm.render ==
          """<form hx-post="/search" hx-target="find #results" hx-swap="innerHTML settle:200ms" hx-trigger="submit" hx-indicator="#spinner"><input type="text" name="q" hx-get="/suggest" hx-trigger="input changed delay:300ms" hx-target="next .suggestions"/><button type="submit">Search</button></form>"""
      )
    },
    test("renders the infinite scroll example") {
      val nextPage       = 2
      val infiniteScroll = div(
        hxGet := s"/items?page=$nextPage",
        hxTrigger := HxTrigger.revealed,
        hxSwap := HxSwap.AfterEnd,
        hxIndicator := CssSelector.`class`("spinner")
      )("Loading...")

      assertTrue(
        infiniteScroll.render ==
          """<div hx-get="/items?page=2" hx-trigger="revealed" hx-swap="afterend" hx-indicator=".spinner">Loading...</div>"""
      )
    },
    test("renders a composed interaction with hx-on and hx-vals") {
      val composed = form(
        hxPost := "/search",
        hxHeaders := HxHeaders.from(Map("X-Requested-With" -> "htmx")),
        hxOn.submit := Js("validate(event)"),
        hxTarget := HxTarget.css("#results"),
        hxSwap := HxSwap.OuterHTML
      )(
        input(
          name := "q",
          hxGet := "/suggest",
          hxVals := HxVals.from(Map("source" -> "search-form")),
          hxTrigger := HxTrigger.input.changed.delay(Duration.ofMillis(250))
        )
      )

      assertTrue(
        composed.render ==
          """<form hx-post="/search" hx-headers="{&quot;X-Requested-With&quot;:&quot;htmx&quot;}" hx-on:submit="validate(event)" hx-target="#results" hx-swap="outerHTML"><input name="q" hx-get="/suggest" hx-vals="{&quot;source&quot;:&quot;search-form&quot;}" hx-trigger="input changed delay:250ms"/></form>"""
      )
    }
  )
}

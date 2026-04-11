package zio.blocks.htmx

import java.time.Duration

import zio.blocks.template._
import zio.test._

object HxTriggerSpec extends ZIOSpecDefault {
  def spec = suite("HxTrigger")(
    test("renders named trigger forms") {
      assertTrue(
        HxTrigger.click.render == "click",
        HxTrigger.load.render == "load",
        HxTrigger.revealed.render == "revealed",
        HxTrigger.intersect.render == "intersect",
        HxTrigger.every(Duration.ofSeconds(2)).render == "every 2s"
      )
    },
    test("renders modifiers in a deterministic order") {
      val rendered = HxTrigger.click
        .threshold(0.5)
        .queue(HxTrigger.QueueStrategy.Last)
        .consume
        .target("closest .result")
        .from("closest form")
        .throttle(Duration.ofSeconds(1))
        .delay(Duration.ofMillis(500))
        .changed
        .once
        .root("#viewport")
        .render

      assertTrue(
        rendered == "click once changed delay:500ms throttle:1s from:(closest form) target:(closest .result) consume queue:last root:#viewport threshold:0.5"
      )
    },
    test("renders filter and queue syntax") {
      assertTrue(
        HxTrigger.click.filter(Js("ctrlKey")).render == "click[ctrlKey]",
        HxTrigger.click.queue(HxTrigger.QueueStrategy.First).render == "click queue:first"
      )
    },
    test("renders from document and window sources") {
      assertTrue(
        HxTrigger.click.from(HxTrigger.document).render == "click from:document",
        HxTrigger.click.from(HxTrigger.window).render == "click from:window"
      )
    },
    test("renders intersect root and threshold modifiers") {
      assertTrue(
        HxTrigger.intersect.root(CssSelector.id("viewport")).threshold(0.5).render ==
          "intersect root:#viewport threshold:0.5"
      )
    },
    test("renders multiple triggers in one attr") {
      val triggers = HxTrigger(HxTrigger.load, HxTrigger.click.delay(Duration.ofSeconds(1)))

      assertTrue(
        input(hxTrigger := triggers).render ==
          """<input hx-trigger="load, click delay:1s"/>"""
      )
    },
    test("hx-trigger renders typed trigger values") {
      val trigger = HxTrigger("input").changed.delay(Duration.ofMillis(300))

      assertTrue(
        input(hxTrigger := trigger).render ==
          """<input hx-trigger="input changed delay:300ms"/>"""
      )
    }
  )
}

package zio.blocks.htmx

import zio.blocks.schema.Schema
import zio.blocks.template._
import zio.http.URL
import zio.test._

object HtmxAttributeSpec extends ZIOSpecDefault {
  final case class Search(term: String, page: Int)
  object Search {
    implicit val schema: Schema[Search] = Schema.derived
  }

  final case class Swap(mode: String) extends HtmxRendered {
    def render: String = mode
  }

  def spec = suite("HtmxAttribute")(
    test("boolean string attrs render true by default") {
      assertTrue(input(hxBoost).render == """<input hx-boost="true"/>""")
    },
    test("boolean string attrs render false explicitly") {
      assertTrue(input(hxBoost(false)).render == """<input hx-boost="false"/>""")
    },
    test("presence attrs render without a value") {
      assertTrue(input(hxDisable).render == """<input hx-disable/>""")
    },
    test("false-only sentinel attrs only render false") {
      assertTrue(
        input(hxHistory(false)).render == """<input hx-history="false"/>""",
        input(hxHistory(true)).render == """<input/>"""
      )
    },
    test("hx-preserve keeps its distinct presence semantics") {
      assertTrue(
        input(hxPreserve).render == """<input hx-preserve/>""",
        input(hxPreserve(true)).render == """<input hx-preserve="true"/>""",
        input(hxPreserve(false)).render == """<input/>"""
      )
    },
    test("url-or-boolean attrs support both forms") {
      assertTrue(
        input(hxPushUrl := false).render == """<input hx-push-url="false"/>""",
        input(hxPushUrl := (URL.root.relative / "items")).render == """<input hx-push-url="/items"/>"""
      )
    },
    test("string attrs render through the shared attribute model") {
      assertTrue(input(hxConfirm := "Really?").render == """<input hx-confirm="Really?"/>""")
    },
    test("dynamic hx-on attrs render Js values") {
      assertTrue(input(hxOn("custom") := Js("handle()")).render == """<input hx-on:custom="handle()"/>""")
    },
    test("json-like attrs render schema-backed values") {
      assertTrue(
        input(hxVals := HxVals.from(Search("zio", 2))).render ==
          """<input hx-vals="{&quot;term&quot;:&quot;zio&quot;,&quot;page&quot;:2}"/>"""
      )
    },
    test("typed HTMX values render through .render") {
      val hxSwap = new HtmxAttribute("hx-swap")

      assertTrue(input(hxSwap := Swap("innerHTML")).render == """<input hx-swap="innerHTML"/>""")
    }
  )
}

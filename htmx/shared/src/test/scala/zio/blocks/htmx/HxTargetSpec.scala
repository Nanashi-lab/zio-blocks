package zio.blocks.htmx

import zio.blocks.template._
import zio.test._

object HxTargetSpec extends ZIOSpecDefault {
  def spec = suite("HxTarget")(
    test("renders HTMX extended selector forms") {
      assertTrue(
        HxTarget.this_.render == "this",
        HxTarget.closest("form").render == "closest form",
        HxTarget.find(CssSelector.`class`("result")).render == "find .result",
        HxTarget.next.render == "next",
        HxTarget.next(CssSelector.`class`("sibling")).render == "next .sibling",
        HxTarget.previous.render == "previous",
        HxTarget.previous(CssSelector.raw("button.primary")).render == "previous button.primary"
      )
    },
    test("renders plain CSS selectors through css(...) helpers") {
      assertTrue(
        HxTarget.css("#spinner").render == "#spinner",
        HxTarget.css(CssSelector.raw("#spinner, .fallback")).render == "#spinner, .fallback"
      )
    },
    test("hx-target accepts extended selectors") {
      assertTrue(input(hxTarget := HxTarget.closest("form")).render == """<input hx-target="closest form"/>""")
    },
    test("hx-indicator accepts CSS selectors") {
      assertTrue(input(hxIndicator := CssSelector.id("spinner")).render == """<input hx-indicator="#spinner"/>""")
    },
    test("hx-include accepts HTMX target selectors") {
      assertTrue(input(hxInclude := HxTarget.find("input")).render == """<input hx-include="find input"/>""")
    },
    test("hx-disabled-elt accepts extended selectors") {
      assertTrue(input(hxDisabledElt := HxTarget.previous(".field")).render == """<input hx-disabled-elt="previous .field"/>""")
    },
    test("hx-select accepts CSS selectors") {
      assertTrue(input(hxSelect := CssSelector.`class`("result")).render == """<input hx-select=".result"/>""")
    },
    test("hx-select-oob accepts CSS selectors") {
      assertTrue(input(hxSelectOob := CssSelector.raw("#main, .flash")).render == """<input hx-select-oob="#main, .flash"/>""")
    }
  )
}

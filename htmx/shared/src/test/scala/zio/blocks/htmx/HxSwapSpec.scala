package zio.blocks.htmx

import java.time.Duration

import zio.blocks.template._
import zio.test._

object HxSwapSpec extends ZIOSpecDefault {
  def spec = suite("HxSwap")(
    test("renders base swap strategies") {
      assertTrue(
        HxSwap.InnerHTML.render == "innerHTML",
        HxSwap.OuterHTML.render == "outerHTML",
        HxSwap.TextContent.render == "textContent",
        HxSwap.BeforeBegin.render == "beforebegin",
        HxSwap.AfterBegin.render == "afterbegin",
        HxSwap.BeforeEnd.render == "beforeend",
        HxSwap.AfterEnd.render == "afterend",
        HxSwap.Delete.render == "delete",
        HxSwap.None.render == "none"
      )
    },
    test("renders modifiers in a deterministic order") {
      val rendered = HxSwap.InnerHTML
        .focusScroll(false)
        .show(HxSwap.ShowPosition.window(HxSwap.Position.Bottom))
        .swap(Duration.ofSeconds(1))
        .ignoreTitle
        .scroll(HxSwap.ScrollPosition.selector("#results", HxSwap.Position.Top))
        .transition
        .settle(Duration.ofMillis(500))
        .render

      assertTrue(
        rendered == "innerHTML swap:1s settle:500ms transition:true scroll:#results:top show:window:bottom ignoreTitle:true focus-scroll:false"
      )
    },
    test("renders show and scroll special forms") {
      assertTrue(
        HxSwap.InnerHTML.show(HxSwap.ShowPosition.None).render == "innerHTML show:none",
        HxSwap.InnerHTML.show(HxSwap.ShowPosition.selector("#details", HxSwap.Position.Top)).render ==
          "innerHTML show:#details:top",
        HxSwap.InnerHTML.scroll(HxSwap.ScrollPosition.window(HxSwap.Position.Bottom)).render ==
          "innerHTML scroll:window:bottom"
      )
    },
    test("hx-swap renders typed swap values") {
      val value = HxSwap.InnerHTML.swap(Duration.ofSeconds(1)).settle(Duration.ofMillis(500))

      assertTrue(input(hxSwap := value).render == """<input hx-swap="innerHTML swap:1s settle:500ms"/>""")
    },
    test("hx-swap-oob renders boolean and swap strategy forms") {
      assertTrue(
        input(hxSwapOob := true).render == """<input hx-swap-oob="true"/>""",
        input(hxSwapOob := HxSwap.AfterEnd).render == """<input hx-swap-oob="afterend"/>"""
      )
    },
    test("hx-swap-oob renders swap and selector forms") {
      val swap = HxSwap.BeforeEnd.show(HxSwap.ShowPosition.selector(CssSelector.id("panel"), HxSwap.Position.Bottom))

      assertTrue(
        input(hxSwapOob := swap.oob(CssSelector.`class`("flash"))).render ==
          """<input hx-swap-oob="beforeend show:#panel:bottom:.flash"/>"""
      )
    }
  )
}

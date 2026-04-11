package zio.blocks.htmx

import java.time.Duration

import zio.blocks.template._
import zio.http.URL
import zio.test._

object HtmxAdvancedAttributeSpec extends ZIOSpecDefault {
  def spec = suite("HtmxAdvancedAttribute")(
    test("renders hx-history-elt and hx-validate") {
      assertTrue(
        input(hxHistoryElt).render == """<input hx-history-elt/>""",
        input(hxValidate).render == """<input hx-validate="true"/>""",
        input(hxValidate(false)).render == """<input hx-validate="false"/>"""
      )
    },
    test("renders prompt and encoding attrs") {
      assertTrue(
        input(hxPrompt := "Really?").render == """<input hx-prompt="Really?"/>""",
        form(hxEncoding := HxEncoding.Multipart).render == """<form hx-encoding="multipart/form-data"></form>"""
      )
    },
    test("renders hx-params forms") {
      assertTrue(
        input(hxParams := HxParams.All).render == """<input hx-params="*"/>""",
        input(hxParams := HxParams.None).render == """<input hx-params="none"/>""",
        input(hxParams := HxParams.Not("token", "debug")).render == """<input hx-params="not token,debug"/>""",
        input(hxParams := HxParams.Only("q", "page")).render == """<input hx-params="q,page"/>"""
      )
    },
    test("renders hx-request JSON") {
      val request = HxRequest(timeout = Some(Duration.ofMillis(100)), credentials = Some(true), noHeaders = Some(false))

      assertTrue(
        div(hxRequest := request).render ==
          """<div hx-request="{&quot;timeout&quot;:100,&quot;credentials&quot;:true,&quot;noHeaders&quot;:false}"></div>"""
      )
    },
    test("renders hx-sync strategies") {
      assertTrue(
        input(hxSync := HxSync.closest("form").abort).render == """<input hx-sync="closest form:abort"/>""",
        input(hxSync := HxSync.this_.replace).render == """<input hx-sync="this:replace"/>""",
        input(hxSync := HxSync.css(CssSelector.id("search")).queueLast).render ==
          """<input hx-sync="#search:queue last"/>"""
      )
    },
    test("renders inherit and disinherit attrs") {
      assertTrue(
        div(hxInherit := HxInherit.all).render == """<div hx-inherit="*"></div>""",
        div(hxInherit := HxInherit.only("hx-target", "hx-select")).render ==
          """<div hx-inherit="hx-target hx-select"></div>""",
        div(hxDisinherit := HxDisinherit.all).render == """<div hx-disinherit="*"></div>""",
        div(hxDisinherit := HxDisinherit.only("hx-target", "hx-select")).render ==
          """<div hx-disinherit="hx-target hx-select"></div>"""
      )
    },
    test("renders hx-ext with comma-separated and ignore forms") {
      val ext = HxExt("preload", "morph").andIgnore("debug")

      assertTrue(
        div(hxExt := ext).render == """<div hx-ext="preload,morph,ignore:debug"></div>""",
        div(hxExt := HxExt.ignore("preload")).render == """<div hx-ext="ignore:preload"></div>"""
      )
    },
    test("renders inherit-aware include and disabled selectors") {
      assertTrue(
        input(hxInclude := HxSelectorList.inherit(HxTarget.find("input"), HxTarget.css("[name='email']"))).render ==
          """<input hx-include="inherit, find input, [name=&#x27;email&#x27;]"/>""",
        input(hxDisabledElt := HxSelectorList.inherit(HxTarget.find("input[type='text']"), HxTarget.find("button"))).render ==
          """<input hx-disabled-elt="inherit, find input[type=&#x27;text&#x27;], find button"/>"""
      )
    },
    test("renders hx-select-oob with multiple selector entries") {
      val selectOob = HxSelectOob(
        HxSelectOob.selector(CssSelector.id("alert")),
        HxSelectOob.selector(CssSelector.id("details"), HxSwap.AfterBegin)
      )

      assertTrue(
        button(hxGet := URL.root.relative / "info", hxSelectOob := selectOob).render ==
          """<button hx-get="/info" hx-select-oob="#alert, #details:afterbegin"></button>"""
      )
    }
  )
}

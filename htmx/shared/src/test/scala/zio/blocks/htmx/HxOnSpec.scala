package zio.blocks.htmx

import zio.blocks.template._
import zio.test._

object HxOnSpec extends ZIOSpecDefault {
  def spec = suite("HxOn")(
    test("renders common event sugar") {
      assertTrue(
        input(hxOn.click := Js("doSomething()")).render == """<input hx-on:click="doSomething()"/>""",
        input(hxOn.submit := Js("validate()")).render == """<input hx-on:submit="validate()"/>""",
        input(hxOn.input := Js("suggest()")).render == """<input hx-on:input="suggest()"/>""",
        input(hxOn.change := Js("sync()")).render == """<input hx-on:change="sync()"/>""",
        input(hxOn.load := Js("init()")).render == """<input hx-on:load="init()"/>"""
      )
    },
    test("dynamic fallback still works") {
      assertTrue(
        input(hxOn("custom-event") := Js("handle()")).render ==
          """<input hx-on:custom-event="handle()"/>"""
      )
    }
  )
}

package zio.blocks.htmx

import scala.collection.immutable.ListMap

import zio.blocks.schema.Schema
import zio.blocks.template._
import zio.test._

object HxJsonSpec extends ZIOSpecDefault {
  final case class AuthHeaders(requestId: String, retry: Boolean)
  object AuthHeaders {
    implicit val schema: Schema[AuthHeaders] = Schema.derived
  }

  final case class Search(term: String, page: Int, filters: Filters)
  object Search {
    implicit val schema: Schema[Search] = Schema.derived
  }

  final case class Filters(tags: List[String], exact: Boolean)
  object Filters {
    implicit val schema: Schema[Filters] = Schema.derived
  }

  def spec = suite("HxJson")(
    test("renders schema-backed hx-vals for nested case classes") {
      val search = Search("zio", 2, Filters(List("scala", "htmx"), exact = true))

      assertTrue(
        input(hxVals := HxVals.from(search)).render ==
          """<input hx-vals="{&quot;term&quot;:&quot;zio&quot;,&quot;page&quot;:2,&quot;filters&quot;:{&quot;tags&quot;:[&quot;scala&quot;,&quot;htmx&quot;],&quot;exact&quot;:true}}"/>"""
      )
    },
    test("renders hx-headers from maps") {
      val headers = ListMap("X-Request-Id" -> "abc-123", "X-Retry" -> "false")

      assertTrue(
        div(hxHeaders := HxHeaders.from(headers)).render ==
          """<div hx-headers="{&quot;X-Request-Id&quot;:&quot;abc-123&quot;,&quot;X-Retry&quot;:&quot;false&quot;}"></div>"""
      )
    },
    test("renders schema-backed hx-headers") {
      assertTrue(
        div(hxHeaders := HxHeaders.from(AuthHeaders("req-1", retry = false))).render ==
          """<div hx-headers="{&quot;requestId&quot;:&quot;req-1&quot;,&quot;retry&quot;:false}"></div>"""
      )
    },
    test("renders dynamic js: forms") {
      assertTrue(
        input(hxVals := HxVals.js(Js("{page: getPage(), q: event.target.value}"))).render ==
          """<input hx-vals="js:{page: getPage(), q: event.target.value}"/>""",
        div(hxHeaders := HxHeaders.js(Js("{Authorization: token()}"))).render ==
          """<div hx-headers="js:{Authorization: token()}"></div>"""
      )
    },
    test("renders dynamic javascript: forms") {
      assertTrue(
        input(hxVals := HxVals.javascript(Js("payload()"))).render ==
          """<input hx-vals="javascript:payload()"/>"""
      )
    },
    test("escapes rendered JSON inside HTML attributes") {
      assertTrue(
        input(hxVals := HxVals.from(ListMap("quoted" -> "say \"hi\" <ok>"))).render ==
          "<input hx-vals=\"{&quot;quoted&quot;:&quot;say \\&quot;hi\\&quot; \\u003cok\\u003e&quot;}\"/>"
      )
    }
  )
}

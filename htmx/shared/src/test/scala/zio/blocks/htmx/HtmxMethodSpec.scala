package zio.blocks.htmx

import zio.blocks.template._
import zio.http.{Path, QueryParams, Scheme, URL}
import zio.test._

object HtmxMethodSpec extends ZIOSpecDefault {
  def spec = suite("HtmxMethod")(
    test("hx-get renders String relative URLs") {
      assertTrue(input(hxGet := "/search").render == """<input hx-get="/search"/>""")
    },
    test("hx-post renders relative URL values") {
      val url = URL.fromPath(Path.root / "search")

      assertTrue(input(hxPost := url).render == """<input hx-post="/search"/>""")
    },
    test("hx-put renders absolute URL values") {
      val url = URL
        .fromPath(Path.root / "search")
        .scheme(Scheme.HTTPS)
        .host("api.example.com")

      assertTrue(input(hxPut := url).render == """<input hx-put="https://api.example.com/search"/>""")
    },
    test("hx-patch renders encoded query params") {
      val url = URL
        .fromPath(Path.root / "search")
        .addQueryParams(QueryParams("q" -> "zio blocks", "page" -> "2"))

      assertTrue(input(hxPatch := url).render == """<input hx-patch="/search?q=zio%20blocks&amp;page=2"/>""")
    },
    test("hx-delete renders path-built URL values") {
      val url = URL.fromPath(Path.root / "items") / "123"

      assertTrue(input(hxDelete := url).render == """<input hx-delete="/items/123"/>""")
    }
  )
}

package zio.blocks.htmx

class HxOn private[htmx] () {
  def apply(eventName: String): HtmxAttribute =
    new HtmxAttribute(s"hx-on:$eventName")

  val click: HtmxAttribute = apply("click")
  val submit: HtmxAttribute = apply("submit")
  val input: HtmxAttribute = apply("input")
  val change: HtmxAttribute = apply("change")
  val load: HtmxAttribute = apply("load")
}

object HxOn extends HxOn()

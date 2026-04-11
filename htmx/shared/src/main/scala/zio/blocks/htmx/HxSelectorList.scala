package zio.blocks.htmx

import zio.blocks.template.CssSelector

final case class HxSelectorList private[htmx] (inherit: Boolean, selectors: List[String]) extends HtmxRendered {
  def render: String = {
    val values = (if (inherit) List("inherit") else Nil) ::: selectors
    values.mkString(", ")
  }
}

object HxSelectorList {
  def apply(first: HxTarget, rest: HxTarget*): HxSelectorList =
    HxSelectorList(inherit = false, (first +: rest).toList.map(_.render))

  def css(first: CssSelector, rest: CssSelector*): HxSelectorList =
    HxSelectorList(inherit = false, (first +: rest).toList.map(_.render))

  def raw(first: String, rest: String*): HxSelectorList =
    HxSelectorList(inherit = false, first :: rest.toList)

  def inherit(first: HxTarget, rest: HxTarget*): HxSelectorList =
    HxSelectorList(inherit = true, (first +: rest).toList.map(_.render))

  def inherit(first: CssSelector, rest: CssSelector*): HxSelectorList =
    HxSelectorList(inherit = true, (first +: rest).toList.map(_.render))

  def inheritRaw(first: String, rest: String*): HxSelectorList =
    HxSelectorList(inherit = true, first :: rest.toList)
}

package zio.blocks.htmx

import zio.blocks.template.CssSelector

final case class HxSelectOob private[htmx] (entries: List[HxSelectOob.Entry]) extends HtmxRendered {
  def render: String = entries.map(_.render).mkString(", ")
}

object HxSelectOob {
  sealed trait Entry extends HtmxRendered

  object Entry {
    final case class Selector(selector: String, swap: Option[HxSwap]) extends Entry {
      def render: String =
        swap match {
          case Some(value) => s"$selector:${value.render}"
          case scala.None  => selector
        }
    }
  }

  def apply(first: Entry, rest: Entry*): HxSelectOob =
    HxSelectOob(first :: rest.toList)

  def selector(selector: String): Entry =
    Entry.Selector(selector, scala.None)

  def selector(selector: CssSelector): Entry =
    Entry.Selector(selector.render, scala.None)

  def selector(selector: String, swap: HxSwap): Entry =
    Entry.Selector(selector, Some(swap))

  def selector(selector: CssSelector, swap: HxSwap): Entry =
    Entry.Selector(selector.render, Some(swap))
}

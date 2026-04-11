package zio.blocks.htmx

import zio.blocks.template.CssSelector

sealed trait HxTarget extends HtmxRendered

object HxTarget {
  case object This extends HxTarget {
    def render: String = "this"
  }

  final case class Closest(selector: String) extends HxTarget {
    def render: String = s"closest $selector"
  }

  final case class Find(selector: String) extends HxTarget {
    def render: String = s"find $selector"
  }

  case object Next extends HxTarget {
    def render: String = "next"
  }

  final case class NextMatching(selector: String) extends HxTarget {
    def render: String = s"next $selector"
  }

  case object Previous extends HxTarget {
    def render: String = "previous"
  }

  final case class PreviousMatching(selector: String) extends HxTarget {
    def render: String = s"previous $selector"
  }

  final case class Css(selector: String) extends HxTarget {
    def render: String = selector
  }

  def this_ : HxTarget = This

  def closest(selector: String): HxTarget = Closest(selector)

  def closest(selector: CssSelector): HxTarget = closest(selector.render)

  def find(selector: String): HxTarget = Find(selector)

  def find(selector: CssSelector): HxTarget = find(selector.render)

  def next: HxTarget = Next

  def next(selector: String): HxTarget = NextMatching(selector)

  def next(selector: CssSelector): HxTarget = next(selector.render)

  def previous: HxTarget = Previous

  def previous(selector: String): HxTarget = PreviousMatching(selector)

  def previous(selector: CssSelector): HxTarget = previous(selector.render)

  def css(selector: String): HxTarget = Css(selector)

  def css(selector: CssSelector): HxTarget = css(selector.render)
}

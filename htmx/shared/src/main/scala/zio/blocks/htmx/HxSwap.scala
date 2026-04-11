package zio.blocks.htmx

import java.time.Duration

import zio.blocks.template.CssSelector

final case class HxSwap private (
  strategy: HxSwap.Strategy,
  modifiers: HxSwap.Modifiers = HxSwap.Modifiers()
) extends HtmxRendered {
  import HxSwap._

  def swap(duration: Duration): HxSwap =
    copy(modifiers = modifiers.copy(swapDuration = Some(duration)))

  def settle(duration: Duration): HxSwap =
    copy(modifiers = modifiers.copy(settleDuration = Some(duration)))

  def transition: HxSwap =
    copy(modifiers = modifiers.copy(transition = true))

  def scroll(position: ScrollPosition): HxSwap =
    copy(modifiers = modifiers.copy(scroll = Some(position)))

  def show(position: ShowPosition): HxSwap =
    copy(modifiers = modifiers.copy(show = Some(position)))

  def ignoreTitle: HxSwap =
    copy(modifiers = modifiers.copy(ignoreTitle = true))

  def focusScroll(enabled: Boolean): HxSwap =
    copy(modifiers = modifiers.copy(focusScroll = Some(enabled)))

  def oob(selector: String): HxSwap.Oob =
    HxSwap.Oob(this, selector)

  def oob(selector: CssSelector): HxSwap.Oob =
    oob(selector.render)

  def render: String = {
    val builder = List.newBuilder[String]
    builder += strategy.render
    modifiers.swapDuration.foreach(duration => builder += s"swap:${formatDuration(duration)}")
    modifiers.settleDuration.foreach(duration => builder += s"settle:${formatDuration(duration)}")
    if (modifiers.transition) builder += "transition:true"
    modifiers.scroll.foreach(position => builder += s"scroll:${position.render}")
    modifiers.show.foreach(position => builder += s"show:${position.render}")
    if (modifiers.ignoreTitle) builder += "ignoreTitle:true"
    modifiers.focusScroll.foreach(enabled => builder += s"focus-scroll:${renderBoolean(enabled)}")
    builder.result().mkString(" ")
  }
}

object HxSwap {
  sealed trait Strategy extends Product with Serializable {
    def render: String
  }

  object Strategy {
    case object InnerHTML   extends Strategy { def render: String = "innerHTML" }
    case object OuterHTML   extends Strategy { def render: String = "outerHTML" }
    case object TextContent extends Strategy { def render: String = "textContent" }
    case object BeforeBegin extends Strategy { def render: String = "beforebegin" }
    case object AfterBegin  extends Strategy { def render: String = "afterbegin" }
    case object BeforeEnd   extends Strategy { def render: String = "beforeend" }
    case object AfterEnd    extends Strategy { def render: String = "afterend" }
    case object Delete      extends Strategy { def render: String = "delete" }
    case object None_       extends Strategy { def render: String = "none" }
  }

  final case class Modifiers(
    swapDuration: Option[Duration] = scala.None,
    settleDuration: Option[Duration] = scala.None,
    transition: Boolean = false,
    scroll: Option[ScrollPosition] = scala.None,
    show: Option[ShowPosition] = scala.None,
    ignoreTitle: Boolean = false,
    focusScroll: Option[Boolean] = scala.None
  )

  sealed trait Position extends Product with Serializable {
    def render: String
  }

  object Position {
    case object Top extends Position {
      def render: String = "top"
    }

    case object Bottom extends Position {
      def render: String = "bottom"
    }
  }

  sealed trait Target extends Product with Serializable {
    def render: String
  }

  object Target {
    case object Window extends Target {
      def render: String = "window"
    }

    final case class Selector(value: String) extends Target {
      def render: String = value
    }
  }

  sealed trait ScrollPosition extends HtmxRendered

  object ScrollPosition {
    final case class Current(position: Position) extends ScrollPosition {
      def render: String = position.render
    }

    final case class Targeted(target: Target, position: Position) extends ScrollPosition {
      def render: String = s"${target.render}:${position.render}"
    }

    def top: ScrollPosition = Current(Position.Top)

    def bottom: ScrollPosition = Current(Position.Bottom)

    def window(position: Position): ScrollPosition =
      Targeted(Target.Window, position)

    def selector(selector: String, position: Position): ScrollPosition =
      Targeted(Target.Selector(selector), position)

    def selector(cssSelector: CssSelector, position: Position): ScrollPosition =
      selector(cssSelector.render, position)
  }

  sealed trait ShowPosition extends HtmxRendered

  object ShowPosition {
    case object None extends ShowPosition {
      def render: String = "none"
    }

    final case class Current(position: Position) extends ShowPosition {
      def render: String = position.render
    }

    final case class Targeted(target: Target, position: Position) extends ShowPosition {
      def render: String = s"${target.render}:${position.render}"
    }

    def top: ShowPosition = Current(Position.Top)

    def bottom: ShowPosition = Current(Position.Bottom)

    def window(position: Position): ShowPosition =
      Targeted(Target.Window, position)

    def selector(selector: String, position: Position): ShowPosition =
      Targeted(Target.Selector(selector), position)

    def selector(cssSelector: CssSelector, position: Position): ShowPosition =
      selector(cssSelector.render, position)
  }

  final case class Oob private[htmx] (swap: HxSwap, selector: String) extends HtmxRendered {
    def render: String = s"${swap.render}:$selector"
  }

  val InnerHTML: HxSwap = HxSwap(Strategy.InnerHTML)
  val OuterHTML: HxSwap = HxSwap(Strategy.OuterHTML)
  val TextContent: HxSwap = HxSwap(Strategy.TextContent)
  val BeforeBegin: HxSwap = HxSwap(Strategy.BeforeBegin)
  val AfterBegin: HxSwap = HxSwap(Strategy.AfterBegin)
  val BeforeEnd: HxSwap = HxSwap(Strategy.BeforeEnd)
  val AfterEnd: HxSwap = HxSwap(Strategy.AfterEnd)
  val Delete: HxSwap = HxSwap(Strategy.Delete)
  val None: HxSwap = HxSwap(Strategy.None_)

  private[htmx] def renderBoolean(value: Boolean): String =
    if (value) "true" else "false"

  private[htmx] def formatDuration(duration: Duration): String = {
    val nanos = duration.toNanos

    if (nanos % 1000000000L == 0L) s"${nanos / 1000000000L}s"
    else if (nanos % 1000000L == 0L) s"${nanos / 1000000L}ms"
    else {
      val millis = BigDecimal(nanos) / BigDecimal(1000000)
      s"${millis.bigDecimal.stripTrailingZeros.toPlainString}ms"
    }
  }
}

final class HtmxSwapOobAttribute private[htmx] (val name: String) {
  def :=(value: Boolean): zio.blocks.template.Dom.Attribute =
    HtmxValue.booleanString.render(name, value)

  def :=(value: HxSwap): zio.blocks.template.Dom.Attribute =
    HtmxValue.string.contramap[HxSwap](_.render).render(name, value)

  def :=(value: HxSwap.Oob): zio.blocks.template.Dom.Attribute =
    HtmxValue.string.contramap[HxSwap.Oob](_.render).render(name, value)
}

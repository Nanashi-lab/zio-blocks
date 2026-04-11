package zio.blocks.htmx

import java.time.Duration

import zio.blocks.template.{CssSelector, Js}

final case class HxTrigger private (
  event: HxTrigger.Event,
  filterExpression: Option[Js] = scala.None,
  modifiers: HxTrigger.Modifiers = HxTrigger.Modifiers()
) extends HtmxRendered {
  import HxTrigger._

  def once: HxTrigger =
    copy(modifiers = modifiers.copy(once = true))

  def changed: HxTrigger =
    copy(modifiers = modifiers.copy(changed = true))

  def delay(duration: Duration): HxTrigger =
    copy(modifiers = modifiers.copy(delay = Some(duration)))

  def throttle(duration: Duration): HxTrigger =
    copy(modifiers = modifiers.copy(throttle = Some(duration)))

  def from(source: Source): HxTrigger =
    copy(modifiers = modifiers.copy(from = Some(source)))

  def from(selector: String): HxTrigger =
    from(Source.selector(selector))

  def from(selector: CssSelector): HxTrigger =
    from(selector.render)

  def target(selector: String): HxTrigger =
    copy(modifiers = modifiers.copy(target = Some(renderSelector(selector))))

  def target(selector: CssSelector): HxTrigger =
    target(selector.render)

  def consume: HxTrigger =
    copy(modifiers = modifiers.copy(consume = true))

  def queue(strategy: QueueStrategy): HxTrigger =
    copy(modifiers = modifiers.copy(queue = Some(strategy)))

  def filter(expression: Js): HxTrigger =
    copy(filterExpression = Some(expression))

  def root(selector: String): HxTrigger =
    copy(modifiers = modifiers.copy(root = Some(renderSelector(selector))))

  def root(selector: CssSelector): HxTrigger =
    root(selector.render)

  def threshold(value: Double): HxTrigger =
    copy(modifiers = modifiers.copy(threshold = Some(value)))

  def render: String = {
    val builder = List.newBuilder[String]
    val base    = event.render + filterExpression.fold("")(expression => s"[${expression.value}]")

    builder += base
    if (modifiers.once) builder += "once"
    if (modifiers.changed) builder += "changed"
    modifiers.delay.foreach(duration => builder += s"delay:${HxSwap.formatDuration(duration)}")
    modifiers.throttle.foreach(duration => builder += s"throttle:${HxSwap.formatDuration(duration)}")
    modifiers.from.foreach(source => builder += s"from:${source.render}")
    modifiers.target.foreach(selector => builder += s"target:$selector")
    if (modifiers.consume) builder += "consume"
    modifiers.queue.foreach(strategy => builder += s"queue:${strategy.render}")
    modifiers.root.foreach(selector => builder += s"root:$selector")
    modifiers.threshold.foreach(value => builder += s"threshold:${renderDecimal(value)}")
    builder.result().mkString(" ")
  }
}

object HxTrigger {
  sealed trait Event extends Product with Serializable {
    def render: String
  }

  object Event {
    final case class Named(name: String) extends Event {
      def render: String = name
    }

    final case class Every(duration: Duration) extends Event {
      def render: String = s"every ${HxSwap.formatDuration(duration)}"
    }
  }

  final case class Modifiers(
    once: Boolean = false,
    changed: Boolean = false,
    delay: Option[Duration] = scala.None,
    throttle: Option[Duration] = scala.None,
    from: Option[Source] = scala.None,
    target: Option[String] = scala.None,
    consume: Boolean = false,
    queue: Option[QueueStrategy] = scala.None,
    root: Option[String] = scala.None,
    threshold: Option[Double] = scala.None
  )

  sealed trait Source extends Product with Serializable {
    def render: String
  }

  object Source {
    case object Document extends Source {
      def render: String = "document"
    }

    case object Window extends Source {
      def render: String = "window"
    }

    final case class Selector(value: String) extends Source {
      def render: String = value
    }

    def selector(value: String): Source =
      Selector(renderSelector(value))

    def selector(value: CssSelector): Source =
      selector(value.render)
  }

  sealed trait QueueStrategy extends Product with Serializable {
    def render: String
  }

  object QueueStrategy {
    case object First extends QueueStrategy {
      def render: String = "first"
    }

    case object Last extends QueueStrategy {
      def render: String = "last"
    }

    case object All extends QueueStrategy {
      def render: String = "all"
    }

    case object None_ extends QueueStrategy {
      def render: String = "none"
    }
  }

  final case class Combined private[htmx] (triggers: List[HxTrigger]) extends HtmxRendered {
    def render: String = triggers.map(_.render).mkString(", ")
  }

  def apply(eventName: String): HxTrigger =
    HxTrigger(Event.Named(eventName))

  def apply(first: HxTrigger, rest: HxTrigger*): Combined =
    Combined(first :: rest.toList)

  def every(duration: Duration): HxTrigger =
    HxTrigger(Event.Every(duration))

  val click: HxTrigger     = HxTrigger("click")
  val load: HxTrigger      = HxTrigger("load")
  val revealed: HxTrigger  = HxTrigger("revealed")
  val intersect: HxTrigger = HxTrigger("intersect")
  val submit: HxTrigger    = HxTrigger("submit")
  val input: HxTrigger     = HxTrigger("input")
  val change: HxTrigger    = HxTrigger("change")

  val document: Source = Source.Document
  val window: Source   = Source.Window

  private[htmx] def renderSelector(selector: String): String =
    if (selector.exists(_.isWhitespace)) s"($selector)" else selector

  private[htmx] def renderDecimal(value: Double): String =
    BigDecimal(value).bigDecimal.stripTrailingZeros.toPlainString
}

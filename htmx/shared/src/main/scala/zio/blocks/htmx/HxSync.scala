package zio.blocks.htmx

import zio.blocks.template.CssSelector

final case class HxSync private (
  target: HxTarget,
  strategy: Option[HxSync.Strategy] = scala.None
) extends HtmxRendered {
  import HxSync._

  def drop: HxSync = copy(strategy = Some(Strategy.Drop))

  def abort: HxSync = copy(strategy = Some(Strategy.Abort))

  def replace: HxSync = copy(strategy = Some(Strategy.Replace))

  def queue: HxSync = copy(strategy = Some(Strategy.Queue))

  def queueFirst: HxSync = copy(strategy = Some(Strategy.QueueFirst))

  def queueLast: HxSync = copy(strategy = Some(Strategy.QueueLast))

  def queueAll: HxSync = copy(strategy = Some(Strategy.QueueAll))

  def render: String =
    strategy match {
      case Some(value) => s"${target.render}:${value.render}"
      case scala.None  => target.render
    }
}

object HxSync {
  sealed trait Strategy extends Product with Serializable {
    def render: String
  }

  object Strategy {
    case object Drop extends Strategy {
      def render: String = "drop"
    }

    case object Abort extends Strategy {
      def render: String = "abort"
    }

    case object Replace extends Strategy {
      def render: String = "replace"
    }

    case object Queue extends Strategy {
      def render: String = "queue"
    }

    case object QueueFirst extends Strategy {
      def render: String = "queue first"
    }

    case object QueueLast extends Strategy {
      def render: String = "queue last"
    }

    case object QueueAll extends Strategy {
      def render: String = "queue all"
    }
  }

  def this_ : HxSync = HxSync(HxTarget.this_)

  def closest(selector: String): HxSync = HxSync(HxTarget.closest(selector))

  def closest(selector: CssSelector): HxSync = HxSync(HxTarget.closest(selector))

  def find(selector: String): HxSync = HxSync(HxTarget.find(selector))

  def find(selector: CssSelector): HxSync = HxSync(HxTarget.find(selector))

  def next: HxSync = HxSync(HxTarget.next)

  def next(selector: String): HxSync = HxSync(HxTarget.next(selector))

  def next(selector: CssSelector): HxSync = HxSync(HxTarget.next(selector))

  def previous: HxSync = HxSync(HxTarget.previous)

  def previous(selector: String): HxSync = HxSync(HxTarget.previous(selector))

  def previous(selector: CssSelector): HxSync = HxSync(HxTarget.previous(selector))

  def css(selector: String): HxSync = HxSync(HxTarget.css(selector))

  def css(selector: CssSelector): HxSync = HxSync(HxTarget.css(selector))
}

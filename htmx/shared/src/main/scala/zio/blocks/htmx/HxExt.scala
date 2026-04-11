package zio.blocks.htmx

final case class HxExt private[htmx] (entries: List[HxExt.Entry]) extends HtmxRendered {
  def and(name: String): HxExt =
    copy(entries = entries :+ HxExt.Entry.Use(name))

  def andIgnore(name: String): HxExt =
    copy(entries = entries :+ HxExt.Entry.Ignore(name))

  def render: String = entries.map(_.render).mkString(",")
}

object HxExt {
  sealed trait Entry extends HtmxRendered

  object Entry {
    final case class Use(name: String) extends Entry {
      def render: String = name
    }

    final case class Ignore(name: String) extends Entry {
      def render: String = s"ignore:$name"
    }
  }

  def apply(first: String, rest: String*): HxExt =
    HxExt((first :: rest.toList).map(Entry.Use.apply))

  def ignore(name: String): HxExt =
    HxExt(List(Entry.Ignore(name)))
}

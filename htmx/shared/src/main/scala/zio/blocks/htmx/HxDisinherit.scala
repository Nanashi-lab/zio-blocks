package zio.blocks.htmx

sealed trait HxDisinherit extends HtmxRendered

object HxDisinherit {
  private final case class All() extends HxDisinherit {
    def render: String = "*"
  }

  final case class Only private[htmx] (names: List[String]) extends HxDisinherit {
    def render: String = names.mkString(" ")
  }

  val all: HxDisinherit = All()

  def only(first: String, rest: String*): HxDisinherit =
    Only(first :: rest.toList)
}

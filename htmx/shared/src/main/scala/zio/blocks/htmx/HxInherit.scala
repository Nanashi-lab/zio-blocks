package zio.blocks.htmx

sealed trait HxInherit extends HtmxRendered

object HxInherit {
  private final case class All() extends HxInherit {
    def render: String = "*"
  }

  final case class Only private[htmx] (names: List[String]) extends HxInherit {
    def render: String = names.mkString(" ")
  }

  val all: HxInherit = All()

  def only(first: String, rest: String*): HxInherit =
    Only(first :: rest.toList)
}

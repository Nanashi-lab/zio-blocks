package zio.blocks.htmx

sealed trait HxParams extends HtmxRendered

object HxParams {
  case object All extends HxParams {
    def render: String = "*"
  }

  case object None_ extends HxParams {
    def render: String = "none"
  }

  final case class Not private[htmx] (names: List[String]) extends HxParams {
    def render: String = s"not ${names.mkString(",")}"
  }

  final case class Only private[htmx] (names: List[String]) extends HxParams {
    def render: String = names.mkString(",")
  }

  def Not(first: String, rest: String*): HxParams =
    Not(first :: rest.toList)

  def Only(first: String, rest: String*): HxParams =
    Only(first :: rest.toList)

  val None: HxParams = None_
}

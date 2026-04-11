package zio.blocks.htmx

import zio.blocks.template.{Js, ToJs}

sealed trait HxJson extends HtmxRendered

object HxJson {
  sealed trait Prefix extends Product with Serializable {
    def render: String
  }

  object Prefix {
    case object Js extends Prefix {
      def render: String = "js:"
    }

    case object JavaScript extends Prefix {
      def render: String = "javascript:"
    }
  }

  final case class Static private[htmx] (value: String) extends HxJson {
    def render: String = value
  }

  final case class Dynamic private[htmx] (prefix: Prefix, expression: Js) extends HxJson {
    def render: String = prefix.render + expression.value
  }

  def from[A](value: A)(implicit toJs: ToJs[A]): HxJson =
    Static(toJs.toJs(value))

  def js(expression: Js): HxJson =
    Dynamic(Prefix.Js, expression)

  def javascript(expression: Js): HxJson =
    Dynamic(Prefix.JavaScript, expression)
}

object HxVals {
  def from[A](value: A)(implicit toJs: ToJs[A]): HxJson =
    HxJson.from(value)

  def js(expression: Js): HxJson =
    HxJson.js(expression)

  def javascript(expression: Js): HxJson =
    HxJson.javascript(expression)
}

object HxHeaders {
  def from[A](value: A)(implicit toJs: ToJs[A]): HxJson =
    HxJson.from(value)

  def js(expression: Js): HxJson =
    HxJson.js(expression)

  def javascript(expression: Js): HxJson =
    HxJson.javascript(expression)
}

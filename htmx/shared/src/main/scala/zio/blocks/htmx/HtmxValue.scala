package zio.blocks.htmx

import zio.blocks.template.{CssSelector, Dom, Js}
import zio.http.URL

trait HtmxRendered {
  def render: String
}

trait HtmxValue[-A] { self =>
  def render(name: String, value: A): Dom.Attribute

  final def contramap[B](f: B => A): HtmxValue[B] =
    new HtmxValue[B] {
      def render(name: String, value: B): Dom.Attribute = self.render(name, f(value))
    }
}

object HtmxValue {
  def apply[A](implicit ev: HtmxValue[A]): HtmxValue[A] = ev

  implicit val string: HtmxValue[String] =
    new HtmxValue[String] {
      def render(name: String, value: String): Dom.Attribute =
        keyValue(name, Dom.AttributeValue.StringValue(value))
    }

  implicit val url: HtmxValue[URL] =
    string.contramap(_.encode)

  implicit val js: HtmxValue[Js] =
    new HtmxValue[Js] {
      def render(name: String, value: Js): Dom.Attribute =
        keyValue(name, Dom.AttributeValue.JsValue(value))
    }

  implicit val cssSelector: HtmxValue[CssSelector] =
    string.contramap(_.render)

  implicit def rendered[A <: HtmxRendered]: HtmxValue[A] =
    string.contramap(_.render)

  val booleanString: HtmxValue[Boolean] =
    string.contramap(value => if (value) "true" else "false")

  val presence: HtmxValue[Unit] =
    new HtmxValue[Unit] {
      def render(name: String, value: Unit): Dom.Attribute =
        Dom.boolAttr(name)
    }

  val falseOnlySentinel: HtmxValue[Boolean] =
    new HtmxValue[Boolean] {
      def render(name: String, value: Boolean): Dom.Attribute =
        if (value) omit(name)
        else keyValue(name, Dom.AttributeValue.StringValue("false"))
    }

  val stringTrue: HtmxValue[Boolean] =
    new HtmxValue[Boolean] {
      def render(name: String, value: Boolean): Dom.Attribute =
        if (value) keyValue(name, Dom.AttributeValue.StringValue("true"))
        else omit(name)
    }

  private[htmx] def keyValue(name: String, value: Dom.AttributeValue): Dom.Attribute =
    Dom.Attribute.KeyValue(name, value)

  private[htmx] def omit(name: String): Dom.Attribute =
    Dom.boolAttr(name, enabled = false)
}

final class HtmxAttribute(val name: String) {
  def :=[A](value: A)(implicit htmxValue: HtmxValue[A]): Dom.Attribute =
    htmxValue.render(name, value)
}

final class HtmxBooleanStringAttribute private[htmx] (val name: String) {
  def :=(value: Boolean): Dom.Attribute =
    HtmxValue.booleanString.render(name, value)

  def apply(): Dom.Attribute =
    :=(true)
}

final class HtmxPresenceAttribute private[htmx] (val name: String) {
  def apply(): Dom.Attribute =
    HtmxValue.presence.render(name, ())
}

final class HtmxFalseOnlyAttribute private[htmx] (val name: String) {
  def :=(value: Boolean): Dom.Attribute =
    HtmxValue.falseOnlySentinel.render(name, value)
}

final class HtmxStringTrueAttribute private[htmx] (val name: String) {
  def :=(value: Boolean): Dom.Attribute =
    HtmxValue.stringTrue.render(name, value)
}

final class HtmxUrlOrBooleanAttribute(val name: String) {
  def :=(value: URL): Dom.Attribute =
    HtmxValue.url.render(name, value)

  def :=(value: Boolean): Dom.Attribute =
    HtmxValue.booleanString.render(name, value)
}

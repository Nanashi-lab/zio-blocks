package zio.blocks.htmx.headers

import scala.collection.immutable.ListMap

import zio.blocks.chunk.Chunk
import zio.blocks.htmx.HxSwap
import zio.blocks.schema.json.Json
import zio.blocks.template.ToJs
import zio.http.{Header, URL}

sealed trait HXUrlValue extends Product with Serializable {
  def render: String
}

object HXUrlValue {
  case object False extends HXUrlValue {
    def render: String = "false"
  }

  final case class Url(url: URL) extends HXUrlValue {
    def render: String = url.encode
  }

  def parse(headerName: String, value: String): Either[String, HXUrlValue] =
    value.trim match {
      case "false" => Right(False)
      case other   => URL.parse(other).map(Url(_)).left.map(error => s"Invalid $headerName header: $error")
    }

  def url(url: URL): HXUrlValue = Url(url)
}

final case class HXLocation(
  path: URL,
  source: Option[String] = None,
  event: Option[String] = None,
  target: Option[String] = None,
  swap: Option[HxSwap] = None,
  select: Option[String] = None
) extends Header {
  def headerName: String = HXLocation.name

  def renderedValue: String = HXLocation.render(this)
}

object HXLocation extends Header.Typed[HXLocation] {
  val name: String = "hx-location"

  def parse(value: String): Either[String, HXLocation] = {
    val trimmed = value.trim
    if (trimmed.startsWith("{")) parseJson(trimmed)
    else URL.parse(trimmed).map(HXLocation(_)).left.map(error => s"Invalid $name header: $error")
  }

  def render(h: HXLocation): String = {
    val fields = List(
      Some(HtmxResponseHeaderSupport.jsonField("path", ToJs[String].toJs(h.path.encode))),
      h.source.map(value => HtmxResponseHeaderSupport.jsonField("source", ToJs[String].toJs(value))),
      h.event.map(value => HtmxResponseHeaderSupport.jsonField("event", ToJs[String].toJs(value))),
      h.target.map(value => HtmxResponseHeaderSupport.jsonField("target", ToJs[String].toJs(value))),
      h.swap.map(value => HtmxResponseHeaderSupport.jsonField("swap", ToJs[String].toJs(value.render))),
      h.select.map(value => HtmxResponseHeaderSupport.jsonField("select", ToJs[String].toJs(value)))
    ).flatten

    fields.mkString("{", ",", "}")
  }

  private def parseJson(value: String): Either[String, HXLocation] =
    HtmxResponseHeaderJson.parseObject(name, value).flatMap { jsonObject =>
      for {
        path   <- HtmxResponseHeaderJson.requiredString(name, jsonObject, "path").flatMap(parseUrl(name, _))
        source <- HtmxResponseHeaderJson.optionalString(name, jsonObject, "source")
        event  <- HtmxResponseHeaderJson.optionalString(name, jsonObject, "event")
        target <- HtmxResponseHeaderJson.optionalString(name, jsonObject, "target")
        swap   <- HtmxResponseHeaderJson
                  .optionalString(name, jsonObject, "swap")
                  .flatMap(HtmxResponseHeaderSupport.parseOptionalSwap(name, _))
        select <- HtmxResponseHeaderJson.optionalString(name, jsonObject, "select")
      } yield HXLocation(path, source, event, target, swap, select)
    }

  private def parseUrl(headerName: String, value: String): Either[String, URL] =
    URL.parse(value).left.map(error => s"Invalid $headerName header: $error")
}

final case class HXPushUrl(value: HXUrlValue) extends Header {
  def headerName: String = HXPushUrl.name

  def renderedValue: String = HXPushUrl.render(this)
}

object HXPushUrl extends Header.Typed[HXPushUrl] {
  val name: String = "hx-push-url"

  def parse(value: String): Either[String, HXPushUrl] =
    HXUrlValue.parse(name, value).map(HXPushUrl(_))

  def render(h: HXPushUrl): String = h.value.render
}

final case class HXReplaceUrl(value: HXUrlValue) extends Header {
  def headerName: String = HXReplaceUrl.name

  def renderedValue: String = HXReplaceUrl.render(this)
}

object HXReplaceUrl extends Header.Typed[HXReplaceUrl] {
  val name: String = "hx-replace-url"

  def parse(value: String): Either[String, HXReplaceUrl] =
    HXUrlValue.parse(name, value).map(HXReplaceUrl(_))

  def render(h: HXReplaceUrl): String = h.value.render
}

final case class HXRedirect(url: URL) extends Header {
  def headerName: String = HXRedirect.name

  def renderedValue: String = HXRedirect.render(this)
}

object HXRedirect extends Header.Typed[HXRedirect] {
  val name: String = "hx-redirect"

  def parse(value: String): Either[String, HXRedirect] =
    URL.parse(value.trim).map(HXRedirect(_)).left.map(error => s"Invalid $name header: $error")

  def render(h: HXRedirect): String = h.url.encode
}

final case class HXRefresh(refresh: Boolean = true) extends Header {
  def headerName: String = HXRefresh.name

  def renderedValue: String = HXRefresh.render(this)
}

object HXRefresh extends Header.Typed[HXRefresh] {
  val name: String = "hx-refresh"

  def parse(value: String): Either[String, HXRefresh] =
    HtmxHeaderSupport.parseBoolean(name, value).map(HXRefresh(_))

  def render(h: HXRefresh): String = HtmxHeaderSupport.renderBoolean(h.refresh)
}

final case class HXReswap(value: HxSwap) extends Header {
  def headerName: String = HXReswap.name

  def renderedValue: String = HXReswap.render(this)
}

object HXReswap extends Header.Typed[HXReswap] {
  val name: String = "hx-reswap"

  def parse(value: String): Either[String, HXReswap] =
    HtmxResponseHeaderSupport.parseSwap(name, value.trim).map(HXReswap(_))

  def render(h: HXReswap): String = h.value.render
}

final case class HXRetarget(selector: String) extends Header {
  def headerName: String = HXRetarget.name

  def renderedValue: String = HXRetarget.render(this)
}

object HXRetarget extends Header.Typed[HXRetarget] {
  val name: String = "hx-retarget"

  def parse(value: String): Either[String, HXRetarget] =
    Right(HXRetarget(value.trim))

  def render(h: HXRetarget): String = h.selector
}

final case class HXReselect(selector: String) extends Header {
  def headerName: String = HXReselect.name

  def renderedValue: String = HXReselect.render(this)
}

object HXReselect extends Header.Typed[HXReselect] {
  val name: String = "hx-reselect"

  def parse(value: String): Either[String, HXReselect] =
    Right(HXReselect(value.trim))

  def render(h: HXReselect): String = h.selector
}

final case class HXJsonValue private (value: String) extends Product with Serializable {
  def render: String = value
}

object HXJsonValue {
  def from[A](value: A)(implicit toJs: ToJs[A]): HXJsonValue =
    HXJsonValue(toJs.toJs(value))

  def json(value: String): HXJsonValue =
    HXJsonValue(value)
}

sealed trait HXTriggerValue extends Product with Serializable {
  def render: String
}

object HXTriggerValue {
  final case class Names(names: Chunk[String]) extends HXTriggerValue {
    def render: String = names.mkString(", ")
  }

  sealed trait Event extends Product with Serializable {
    def name: String
    def renderField: String
  }

  object Event {
    final case class Detail(name: String, detail: HXJsonValue) extends Event {
      def renderField: String = HtmxResponseHeaderSupport.jsonField(name, detail.render)
    }

    final case class Targeted(name: String, target: String, fields: ListMap[String, HXJsonValue]) extends Event {
      def renderField: String = {
        val renderedFields =
          (List(HtmxResponseHeaderSupport.jsonField("target", ToJs[String].toJs(target))) ++ fields.map {
            case (key, value) =>
              HtmxResponseHeaderSupport.jsonField(key, value.render)
          }).mkString("{", ",", "}")

        HtmxResponseHeaderSupport.jsonField(name, renderedFields)
      }
    }
  }

  final case class Events(events: Chunk[Event]) extends HXTriggerValue {
    def render: String = events.map(_.renderField).mkString("{", ",", "}")
  }

  def names(first: String, rest: String*): HXTriggerValue =
    Names(Chunk.from(first +: rest))

  def detail[A](name: String, value: A)(implicit toJs: ToJs[A]): HXTriggerValue =
    Events(Chunk(Event.Detail(name, HXJsonValue.from(value))))

  def targeted(name: String, target: String, fields: (String, HXJsonValue)*): HXTriggerValue =
    Events(Chunk(Event.Targeted(name, target, ListMap(fields: _*))))

  def parse(headerName: String, value: String): Either[String, HXTriggerValue] = {
    val trimmed = value.trim
    if (trimmed.startsWith("{")) parseJson(headerName, trimmed)
    else {
      val parts = trimmed.split(",").iterator.map(_.trim).filter(_.nonEmpty).toList
      parts match {
        case Nil         => Left(s"Invalid $headerName header: empty trigger value")
        case name :: Nil => Right(Names(Chunk(name)))
        case many        => Right(Names(Chunk.from(many)))
      }
    }
  }

  private def parseJson(headerName: String, value: String): Either[String, HXTriggerValue] =
    HtmxResponseHeaderJson.parseObject(headerName, value).map { jsonObject =>
      val events = jsonObject.value.map { case (eventName, jsonValue) =>
        jsonValue match {
          case Json.Object(fields) =>
            val asMap = ListMap(fields.toList: _*)
            asMap.get("target") match {
              case Some(Json.String(target)) =>
                Event.Targeted(
                  eventName,
                  target,
                  ListMap(asMap.iterator.collect {
                    case (key, value) if key != "target" => key -> HXJsonValue.json(value.print)
                  }.toSeq: _*)
                )
              case _ =>
                Event.Detail(eventName, HXJsonValue.json(jsonValue.print))
            }
          case _ =>
            Event.Detail(eventName, HXJsonValue.json(jsonValue.print))
        }
      }

      Events(events)
    }
}

final case class HXTrigger(value: HXTriggerValue) extends Header {
  def headerName: String = HXTrigger.name

  def renderedValue: String = HXTrigger.render(this)
}

object HXTrigger extends Header.Typed[HXTrigger] {
  val name: String = "hx-trigger"

  def parse(value: String): Either[String, HXTrigger] =
    HXTriggerValue.parse(name, value).map(HXTrigger(_))

  def render(h: HXTrigger): String = h.value.render
}

final case class HXTriggerAfterSettle(value: HXTriggerValue) extends Header {
  def headerName: String = HXTriggerAfterSettle.name

  def renderedValue: String = HXTriggerAfterSettle.render(this)
}

object HXTriggerAfterSettle extends Header.Typed[HXTriggerAfterSettle] {
  val name: String = "hx-trigger-after-settle"

  def parse(value: String): Either[String, HXTriggerAfterSettle] =
    HXTriggerValue.parse(name, value).map(HXTriggerAfterSettle(_))

  def render(h: HXTriggerAfterSettle): String = h.value.render
}

final case class HXTriggerAfterSwap(value: HXTriggerValue) extends Header {
  def headerName: String = HXTriggerAfterSwap.name

  def renderedValue: String = HXTriggerAfterSwap.render(this)
}

object HXTriggerAfterSwap extends Header.Typed[HXTriggerAfterSwap] {
  val name: String = "hx-trigger-after-swap"

  def parse(value: String): Either[String, HXTriggerAfterSwap] =
    HXTriggerValue.parse(name, value).map(HXTriggerAfterSwap(_))

  def render(h: HXTriggerAfterSwap): String = h.value.render
}

private[headers] object HtmxResponseHeaderJson {
  def parseObject(headerName: String, value: String): Either[String, Json.Object] =
    Json.parse(value).left.map(error => s"Invalid $headerName header: ${error.message}").flatMap {
      case objectValue: Json.Object => Right(objectValue)
      case _                        => Left(s"Invalid $headerName header: expected JSON object")
    }

  def requiredString(headerName: String, value: Json.Object, key: String): Either[String, String] =
    optionalString(headerName, value, key).flatMap {
      case Some(fieldValue) => Right(fieldValue)
      case None             => Left(s"Invalid $headerName header: missing '$key'")
    }

  def optionalString(headerName: String, value: Json.Object, key: String): Either[String, Option[String]] = {
    val fields = ListMap(value.value.toList: _*)
    fields.get(key) match {
      case None                    => Right(None)
      case Some(Json.String(text)) => Right(Some(text))
      case Some(other)             => Left(s"Invalid $headerName header: '$key' must be a string, got ${other.jsonType}")
    }
  }
}

private[headers] object HtmxResponseHeaderSupport {
  def jsonField(name: String, value: String): String =
    ToJs[String].toJs(name) + ":" + value

  def parseOptionalSwap(headerName: String, value: Option[String]): Either[String, Option[HxSwap]] =
    value match {
      case Some(swap) => parseSwap(headerName, swap).map(Some(_))
      case None       => Right(None)
    }

  def parseSwap(headerName: String, value: String): Either[String, HxSwap] =
    value match {
      case "innerHTML"   => Right(HxSwap.InnerHTML)
      case "outerHTML"   => Right(HxSwap.OuterHTML)
      case "textContent" => Right(HxSwap.TextContent)
      case "beforebegin" => Right(HxSwap.BeforeBegin)
      case "afterbegin"  => Right(HxSwap.AfterBegin)
      case "beforeend"   => Right(HxSwap.BeforeEnd)
      case "afterend"    => Right(HxSwap.AfterEnd)
      case "delete"      => Right(HxSwap.Delete)
      case "none"        => Right(HxSwap.None)
      case other         => Left(s"Invalid $headerName header: unknown swap strategy '$other'")
    }
}

package zio.blocks.htmx.headers

import zio.http.{Header, URL}

final case class HXRequest(htmx: Boolean) extends Header {
  def headerName: String = HXRequest.name

  def renderedValue: String = HXRequest.render(this)
}

object HXRequest extends Header.Typed[HXRequest] {
  val name: String = "hx-request"

  def parse(value: String): Either[String, HXRequest] =
    HtmxHeaderSupport.parseBoolean(name, value).map(HXRequest(_))

  def render(h: HXRequest): String = HtmxHeaderSupport.renderBoolean(h.htmx)
}

final case class HXBoosted(boosted: Boolean) extends Header {
  def headerName: String = HXBoosted.name

  def renderedValue: String = HXBoosted.render(this)
}

object HXBoosted extends Header.Typed[HXBoosted] {
  val name: String = "hx-boosted"

  def parse(value: String): Either[String, HXBoosted] =
    HtmxHeaderSupport.parseBoolean(name, value).map(HXBoosted(_))

  def render(h: HXBoosted): String = HtmxHeaderSupport.renderBoolean(h.boosted)
}

final case class HXCurrentURL(url: URL) extends Header {
  def headerName: String = HXCurrentURL.name

  def renderedValue: String = HXCurrentURL.render(this)
}

object HXCurrentURL extends Header.Typed[HXCurrentURL] {
  val name: String = "hx-current-url"

  def parse(value: String): Either[String, HXCurrentURL] =
    URL.parse(value.trim).map(HXCurrentURL(_))

  def render(h: HXCurrentURL): String = h.url.encode
}

final case class HXTarget(value: String) extends Header {
  def headerName: String = HXTarget.name

  def renderedValue: String = HXTarget.render(this)
}

object HXTarget extends Header.Typed[HXTarget] {
  val name: String = "hx-target"

  def parse(value: String): Either[String, HXTarget] =
    Right(HXTarget(value.trim))

  def render(h: HXTarget): String = h.value
}

final case class HXTriggerId(value: String) extends Header {
  def headerName: String = HXTriggerId.name

  def renderedValue: String = HXTriggerId.render(this)
}

object HXTriggerId extends Header.Typed[HXTriggerId] {
  val name: String = "hx-trigger"

  def parse(value: String): Either[String, HXTriggerId] =
    Right(HXTriggerId(value.trim))

  def render(h: HXTriggerId): String = h.value
}

final case class HXTriggerName(value: String) extends Header {
  def headerName: String = HXTriggerName.name

  def renderedValue: String = HXTriggerName.render(this)
}

object HXTriggerName extends Header.Typed[HXTriggerName] {
  val name: String = "hx-trigger-name"

  def parse(value: String): Either[String, HXTriggerName] =
    Right(HXTriggerName(value.trim))

  def render(h: HXTriggerName): String = h.value
}

final case class HXHistoryRestoreRequest(restoring: Boolean) extends Header {
  def headerName: String = HXHistoryRestoreRequest.name

  def renderedValue: String = HXHistoryRestoreRequest.render(this)
}

object HXHistoryRestoreRequest extends Header.Typed[HXHistoryRestoreRequest] {
  val name: String = "hx-history-restore-request"

  def parse(value: String): Either[String, HXHistoryRestoreRequest] =
    HtmxHeaderSupport.parseBoolean(name, value).map(HXHistoryRestoreRequest(_))

  def render(h: HXHistoryRestoreRequest): String = HtmxHeaderSupport.renderBoolean(h.restoring)
}

final case class HXPrompt(value: String) extends Header {
  def headerName: String = HXPrompt.name

  def renderedValue: String = HXPrompt.render(this)
}

object HXPrompt extends Header.Typed[HXPrompt] {
  val name: String = "hx-prompt"

  def parse(value: String): Either[String, HXPrompt] =
    Right(HXPrompt(value.trim))

  def render(h: HXPrompt): String = h.value
}

private[headers] object HtmxHeaderSupport {
  def renderBoolean(value: Boolean): String =
    if (value) "true" else "false"

  def parseBoolean(headerName: String, value: String): Either[String, Boolean] =
    value.trim.toLowerCase match {
      case "true"  => Right(true)
      case "false" => Right(false)
      case other    => Left(s"Invalid $headerName header: $other")
    }
}

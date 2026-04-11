package zio.blocks.htmx

import java.time.Duration

final case class HxRequest(
  timeout: Option[Duration] = scala.None,
  credentials: Option[Boolean] = scala.None,
  noHeaders: Option[Boolean] = scala.None
) extends HtmxRendered {
  def render: String = {
    val fields = List(
      timeout.map(value => s"\"timeout\":${value.toMillis}"),
      credentials.map(value => s"\"credentials\":${HxSwap.renderBoolean(value)}"),
      noHeaders.map(value => s"\"noHeaders\":${HxSwap.renderBoolean(value)}")
    ).flatten

    fields.mkString("{", ",", "}")
  }
}

package zio.blocks.htmx

sealed trait HxEncoding extends HtmxRendered

object HxEncoding {
  case object MultipartFormData extends HxEncoding {
    def render: String = "multipart/form-data"
  }

  val Multipart: HxEncoding = MultipartFormData
}

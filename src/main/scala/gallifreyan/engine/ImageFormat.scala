package gallifreyan.engine

sealed abstract class ImageFormat(val extension: String)

object ImageFormat {
  case object PNG extends ImageFormat(".png")
  case object SVG extends ImageFormat(".svg")
  val values: List[ImageFormat] = List(PNG, SVG)
}

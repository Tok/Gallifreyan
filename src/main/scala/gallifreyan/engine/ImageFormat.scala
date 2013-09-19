package gallifreyan.engine

sealed abstract class ImageFormat(val extension: String)

object ImageFormat {
  case object SVG extends ImageFormat(".svg")
  case object PNG extends ImageFormat(".png")
  val values: List[ImageFormat] = List(SVG, PNG)
}

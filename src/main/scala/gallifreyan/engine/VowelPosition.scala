package gallifreyan.engine

sealed abstract class VowelPosition(val offset: Double) {
  val ratio: Double = 0.2D
  val doubleRatio: Double = 0.3D
}

object VowelPosition {
  case object OUT extends VowelPosition(1D)
  case object CENTER extends VowelPosition(0D)
  case object CENTER_IN extends VowelPosition(0D)
  case object IN extends VowelPosition(-1D)
  case object CENTER_OUT extends VowelPosition(0D)
  val values: List[VowelPosition] = List(OUT, CENTER, CENTER_IN, IN, CENTER_OUT)
  def valueOf(name: String): Option[VowelPosition] = values.find(_.toString.equalsIgnoreCase(name))
}

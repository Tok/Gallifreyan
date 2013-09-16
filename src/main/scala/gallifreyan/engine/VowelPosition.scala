package gallifreyan.engine

sealed abstract class VowelPosition(val ratio: Double, val doubleRatio: Double, val offset: Double)

object VowelPosition {
  case object OUT extends VowelPosition(0.1D, 0.15D, 1D)
  case object CENTER extends VowelPosition(0.1D, 0.15D, 0D)
  case object CENTER_IN extends VowelPosition(0.1D, 0.15D, 0D)
  case object IN extends VowelPosition(0.1D, 0.15D, -1D)
  case object CENTER_OUT extends VowelPosition(0.1D, 0.15D, 0D)
  val values: List[VowelPosition] = List(OUT, CENTER, CENTER_IN, IN, CENTER_OUT)
  def valueOf(name: String): Option[VowelPosition] = values.find(_.toString.equalsIgnoreCase(name))
}

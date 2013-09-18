package gallifreyan.engine

sealed abstract class CircleType(val ratio: Double, val doubleRatio: Double, val offset: Double, val isCrossing: Boolean)

object CircleType {
  case object OPEN extends CircleType(0.4D, 0.45D, 0.7D, true)
  case object FULL extends CircleType(0.4D, 0.45D, 0.5D, false)
  case object HALF extends CircleType(0.8D, 0.85D, 1D, true)
  case object STRIKED extends CircleType(0.5D, 0.55D, 1D, false)
  val values: List[CircleType] = List(OPEN, FULL, HALF, STRIKED)
  def valueOf(name: String): Option[CircleType] = values.find(_.toString.equalsIgnoreCase(name))
}

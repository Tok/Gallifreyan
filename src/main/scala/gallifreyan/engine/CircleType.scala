package gallifreyan.engine

sealed abstract class CircleType(val ratio: Double, val doubleRatio: Double, val offset: Double)

object CircleType {
  case object OPEN extends CircleType(0.4D, 0.45D, 0.7D)
  case object FULL extends CircleType(0.4D, 0.45D, 0.5D)
  case object HALF extends CircleType(0.8D, 0.85D, 1.0D)
  case object STRIKED extends CircleType(0.5D, 0.55D, 1.0D)
  val values: List[CircleType] = List(OPEN, FULL, HALF, STRIKED)
  def valueOf(name: String): Option[CircleType] = values.find(_.toString.equalsIgnoreCase(name))
}

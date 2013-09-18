package gallifreyan.engine.cases

case class Connector(val index: Int, val coord: Coord, val distanceToCenter: Double) {
  override def toString: String = index + ">" + coord
}

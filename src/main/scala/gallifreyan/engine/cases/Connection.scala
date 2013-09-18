package gallifreyan.engine.cases

case class Connection(val from: Coord, val to: Coord) {
  override def toString: String = from + "->" + to
}

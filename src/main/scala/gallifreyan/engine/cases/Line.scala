package gallifreyan.engine.cases

case class Line(val start: Coord, val end: Coord) {
  override def toString: String = start + "--" + end
}

package gallifreyan.engine.cases

case class Arc(val circle: Circle, val start: Coord, val end: Coord) {
  override def toString: String = "(" + start + "<" + circle + ">" + end + ")"
}

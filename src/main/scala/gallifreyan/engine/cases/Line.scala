package gallifreyan.engine.cases

case class Line(val start: Coord, val end: Coord) extends Ordered[Line] {
  def compare(that: Line): Int = start.compare(that.start) match {
    case 0 => end.compare(that.end)
    case res: Any => res
  }
  override def toString: String = start + "--" + end
}

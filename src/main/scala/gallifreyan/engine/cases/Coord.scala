package gallifreyan.engine.cases

case class Coord(val x: Int, val y: Int) extends Ordered[Coord] {
  def compare(that: Coord): Int = x.compare(that.x) match {
    case 0 => y.compare(that.y)
    case res: Any => res
  }
  def addToX(v: Int): Coord = Coord(x + v, y)
  def addToY(v: Int): Coord = Coord(x, y + v)
  override def toString: String = x + ":" + y
}

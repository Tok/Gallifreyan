package gallifreyan.shape.parts

import gallifreyan.engine.cases.Coord
import gallifreyan.engine.cases.Circle

case class ArcCircle(val circle: Circle, val starts: List[Coord], val ends: List[Coord]) {
  override def toString: String = circle + " s:" + starts.mkString + " e:" + ends.mkString
}

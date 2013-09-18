package gallifreyan.engine.cases

case class Circle(val center: Coord, val radius: Int) {
  def xStart: Int = center.x - radius
  def yStart: Int = center.y - radius
  def diameter: Double = radius * 2D
  def addToRadius(v: Int): Circle = Circle(center, radius + v)
  override def toString: String = center + "o-" + radius
}

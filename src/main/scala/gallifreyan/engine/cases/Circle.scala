package gallifreyan.engine.cases

case class Circle(val center: Coord, val radius: Int) {
  def xStart: Int = center.x - radius
  def yStart: Int = center.y - radius
  def diameter: Double = radius * 2D
  def addToRadius(v: Int): Circle = Circle(center, radius + v)
  def calcClosestTo(p: Coord): Coord = {
    val xDiff = p.x - center.x
    val yDiff = p.y - center.y
    val mag = Math.sqrt((xDiff * xDiff) + (yDiff * yDiff))
    val x = center.x + (xDiff / mag * radius)
    val y = center.y + (yDiff / mag * radius)
    Coord(x.intValue, y.intValue)
  }
  def calcFarthestFrom(p: Coord): Coord = {
    val closest = calcClosestTo(p)
    val xDiff = (closest.x - center.x)
    val yDiff = (closest.y - center.y)
    Coord(center.x - xDiff, center.y - yDiff)
  }
  def moveFromCenter(p: Coord, mag: Double): Coord = {
    val x = center.x + ((p.x - center.x) * mag)
    val y = center.y + ((p.y - center.y) * mag)
    Coord(x.intValue, y.intValue)
  }
  override def toString: String = center + "o-" + radius
}

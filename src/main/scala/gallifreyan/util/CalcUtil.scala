package gallifreyan.util

import gallifreyan.engine.cases.Circle
import gallifreyan.engine.cases.Coord
import gallifreyan.engine.characters.Consonant
import gallifreyan.engine.CircleType

object CalcUtil {
  def calcDistance(c1: Coord, c2: Coord): Double = {
    val dx = c1.x - c2.x
    val dy = c1.y - c2.y
    Math.sqrt(Math.pow(dx.abs, 2D) + Math.pow(dy.abs, 2D))
  }

  def calcStartAndEnd(c1: Circle, c2: Circle): (Coord, Coord) = {
    calcStartAndEnd(c1.center, c1.radius, c2.center, c2.radius)
  }

  //http://mathworld.wolfram.com/Circle-CircleIntersection.html
  private def calcStartAndEnd(c1: Coord, r1: Double, c2: Coord, r2: Double): (Coord, Coord) = {
    val dist = calcDistance(c1, c2)
    def calcX(): Double = {
      (Math.pow(dist, 2D) - Math.pow(r2, 2D) + Math.pow(r1, 2D)) / (dist * 2D)
    }
    def calcA(): Double = {
      val first = -dist + r2 - r1
      val second = -dist - r2 + r1
      val third = -dist + r2 + r1
      val fourth = dist + r2 + r1
      (1D / dist) * Math.sqrt(first * second * third * fourth)
    }
    val firstX = c1.x - (calcA / 2D)
    val firstY = c1.y + calcX
    val first = Coord(firstX.intValue, firstY.intValue)
    val secondX = c1.x + (calcA / 2D)
    val secondY = c1.y + calcX
    val second = Coord(secondX.intValue, secondY.intValue)
    (first, second)
  }

  def calcAngle(source: Coord, target: Coord): Int = {
    Math.toDegrees(Math.atan2(target.y - source.y, target.x - source.x)).intValue
  }

  //http://mathworld.wolfram.com/CirclePacking.html
  def calcSizeRatio(size: Double): Double = {
    size match {
      case 0 | 1 | 2 => 1D
      case 3 => 2D / (1D + (2D / 3D) * Math.sqrt(3D))
      case 4 => 2D / (1D + Math.sqrt(2D))
      case 5 => 2D / (1D + Math.sqrt(2D * (1D + (1D / Math.sqrt(5D)))))
      case 6 => 2D / 3D
      case 7 => 2D / (1D + (1D / Math.sin(Math.PI / 7)))
      case 8 => 2D / (1D + Math.sqrt(2D * (2D + Math.sqrt(2D))))
      case _ => 0.5D
    }
  }

  def calcLineEnd(circle: Circle, angle: Double, offset: Int): Coord = {
    val x = circle.center.x - (Math.sin(angle) * circle.radius).intValue
    val y = circle.center.y - (Math.cos(angle) * circle.radius).intValue - offset
    Coord(x, y)
  }

  def calcOffsetAndSize(circle: Circle, con: Consonant): (Double, Int) = {
    val isOpenOrFull = con.circleType.equals(CircleType.OPEN) || con.circleType.equals(CircleType.FULL)
    val offset = if (isOpenOrFull) { Math.toRadians(-60D) } else { Math.toRadians(-30D) }
    val size = (circle.radius * 0.07D).intValue
    (offset, size)
  }

  def calcDot(circle: Circle, angle: Double): Coord = {
    val distanceToCenter = circle.radius * 0.85D
    val x = circle.center.x - (Math.sin(angle) * distanceToCenter).intValue
    val y = circle.center.y - (Math.cos(angle) * distanceToCenter).intValue
    Coord(x, y)
  }

  def rotate(coord: Coord, angle: Double, center: Coord): Coord = {
    val xDiff = (coord.x - center.x)
    val yDiff = (coord.y - center.y)
    val cosA = Math.cos(Math.toRadians(angle))
    val sinA = Math.sin(Math.toRadians(angle))
    val x = center.x + (cosA * xDiff) - (sinA * yDiff)
    val y = center.y + (sinA * xDiff) + (cosA * yDiff)
    Coord(x.intValue, y.intValue)
  }

  def calcIntersection(start: Coord, end: Coord, circle: Circle): Coord = {
    val xDiff: Int = end.x - start.x
    val yDiff: Int = end.y - start.y
    val xDiffC: Int = circle.center.x - start.x
    val yDiffC: Int = circle.center.y - start.y
    val div: Double = (xDiff * xDiff) + (yDiff * yDiff)
    val foo: Double = ((xDiff * xDiffC) + (yDiff * yDiffC)) / div
    val sub: Double = ((xDiffC * xDiffC) + (yDiffC * yDiffC) - (circle.radius * circle.radius)) / div
    val disc: Double = (foo * foo) - sub
    if (disc < 0D) { throw new IllegalArgumentException("Line doesn't intersect.") }
    if (disc == 0D) { throw new IllegalArgumentException("Line is tangent.") }
    val scale: Double = -foo - Math.sqrt(disc)
    val x: Double = start.x - (xDiff * scale)
    val y: Double = start.y - (yDiff * scale)
    Coord(x.intValue, y.intValue)
  }
}

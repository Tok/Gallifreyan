package gallifreyan.util

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics2D
import java.awt.Font
import java.awt.geom.Arc2D
import java.awt.geom.Ellipse2D
import java.awt.geom.Rectangle2D
import gallifreyan.engine.traits.Character
import gallifreyan.engine.CircleType
import gallifreyan.engine.cases.Coord
import gallifreyan.engine.MarkType
import gallifreyan.engine.Sentence
import gallifreyan.engine.Size
import gallifreyan.engine.Syllable
import gallifreyan.engine.cases.Circle
import gallifreyan.engine.VowelPosition
import gallifreyan.engine.characters.Consonant
import gallifreyan.engine.characters.Punctation
import gallifreyan.engine.characters.Vowel
import java.awt.geom.Point2D
import gallifreyan.engine.cases.Connector
import gallifreyan.engine.cases.Connection
import gallifreyan.engine.cases.Line

object DrawUtil {
  val conceptMode = true
  val LINE_WIDTH = 2
  val HALF_LINE = LINE_WIDTH / 2
  val STROKE = new BasicStroke(LINE_WIDTH)
  val FONT = new Font(Font.MONOSPACED, Font.BOLD, 30)
  val CONN_MARK_SIZE = 10

  val l: List[Double] = List(0)

  def drawSentence(g2d: Graphics2D, sent: Sentence, fg: Color, bg: Color, addText: Boolean): Unit = {
    //g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    //g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    g2d.setPaint(fg)
    g2d.setBackground(bg)
    g2d.setStroke(STROKE)
    g2d.clearRect(0, 0, Size.width, Size.height)
    drawCircle(g2d, wc, false)
    if (!sent.isEmpty) {
      val rot = 360D / sent.size
      val rots = (1 to sent.size).map(i => 360D - (rot * i)).reverse.toList
      val sizeRatio = CalcUtil.getSizeRatio(sent.size)
      val rotSent = sent.zip(rots)
      val connectorLines: List[Set[Line]] = rotSent.map(si => drawSyllable(g2d, si._1, si._2, sizeRatio))
      markConnections(g2d, connectorLines)
      //val connectorPoints: List[Set[Coord]] = connectorLines.map(_.map(l => l.start))
      //drawConnections(g2d, connectorPoints)
    }
    if (addText) { g2d.drawString(sent.map(_.mkString).mkString, 10, FONT.getSize) }
    g2d.dispose
  }

  private def markConnections(g2d: Graphics2D, lineSets: List[Set[Line]]): Unit = {
    lineSets.map(_.toList).flatten.foreach(l => drawLine(g2d, l.start, l.end))
  }

  private def drawSyllable(g2d: Graphics2D, syl: Syllable, rot: Double, sizeRatio: Double): Set[Line] = {
    def isDouble(i: Int, syl: Syllable): Boolean = i > 0 && syl(i) == syl(i - 1)
    val connectors = syl.indices.map(i => drawCharacter(g2d, syl(i), getSyllableCircle(syl, sizeRatio), isDouble(i, syl), sizeRatio, rot))
    connectors.flatten.toSet
  }

  private def wc(): Circle = {
    val wordCenter = Coord(Size.width / 2, Size.height / 2)
    val wordRadius = (wordCenter.y * 0.7D).intValue
    Circle(wordCenter, wordRadius)
  }

  private def getSyllableCircle(syl: Syllable, sizeRatio: Double): Circle = {
    syl(0) match {
      case con: Consonant => getConsonantCircle(con, wc, false, sizeRatio)
      case _ => getConsonantCircle(Consonant.TH, wc, false, sizeRatio)
      //Vowels in syllables without constants use the same circle as Th
    }
  }

  private def getConsonantCircle(con: Consonant, wordCircle: Circle, isDouble: Boolean, sizeRatio: Double): Circle = {
    val offset = (wordCircle.radius * con.circleType.offset).intValue
    val rat = if (isDouble) { con.circleType.doubleRatio } else { con.circleType.ratio }
    val radius = (wordCircle.radius * rat).intValue
    val fixedRadius = (radius * sizeRatio).intValue
    val fixedCenter = if(con.circleType.equals(CircleType.STRIKED)) {
      wordCircle.center.addToY(wordCircle.radius)
    } else {
      wordCircle.center.addToY(offset).addToY(radius - (fixedRadius))
    }
    Circle(fixedCenter, fixedRadius)
  }

  private def drawCharacter(g2d: Graphics2D, c: Character, sylCircle: Circle, isDouble: Boolean, sizeRatio: Double, rot: Double): Set[Line] = {
    c match {
      case con: Consonant => drawConsonant(g2d, con, isDouble, sizeRatio, -rot)
      case vow: Vowel => drawVowel(g2d, vow, sylCircle, isDouble, -rot)
      case pun: Punctation => drawPunctation(g2d, pun, -rot)
    }
  }

  private def drawConsonant(g2d: Graphics2D, con: Consonant, isDouble: Boolean, sizeRatio: Double, rot: Double): Set[Line] = {
    val original = getConsonantCircle(con, wc, isDouble, sizeRatio)
    def connCircle: Circle = original.addToRadius(CONN_MARK_SIZE)
    val circle = Circle(rotate(original.center, rot), original.radius)
    if (!con.circleType.isCrossing) { drawCircle(g2d, circle, false) } else {
      val (s, e) = CalcUtil.calcStartAndEnd(wc, original)
      drawArc(g2d, circle, rotate(e, rot), rotate(s, rot))
      if (!isDouble) { drawCircle(g2d, circle.addToRadius(-(LINE_WIDTH / 2)), true) }
    }
    if (isDouble) { Set.empty } else {
      def da: Double = Math.toRadians(10D)
      def dda: Double = da * 2D
      con.markType match {
        case MarkType.NONE => Set.empty
        case MarkType.DOUBLE_DOT =>
          val (offset, size) = getOffsetAndSize(circle, con)
          drawConsonantPoint(g2d, rotate(getDot(original, offset + da), rot), size)
          drawConsonantPoint(g2d, rotate(getDot(original, offset - da), rot), size)
          Set.empty
        case MarkType.TRIPPLE_DOT =>
          val (offset, size) = getOffsetAndSize(circle, con)
          drawConsonantPoint(g2d, rotate(getDot(original, offset + dda), rot), size)
          drawConsonantPoint(g2d, rotate(getDot(original, offset), rot), size)
          drawConsonantPoint(g2d, rotate(getDot(original, offset - dda), rot), size)
          Set.empty
        case MarkType.TRIPPLE_LINE =>
          val leftStart = rotate(getLineEnd(original, dda), rot)
          val middleStart = rotate(getLineEnd(original, 0D), rot)
          val rightStart = rotate(getLineEnd(original, -dda), rot)
          val leftEnd = rotate(getLineEnd(connCircle, dda), rot)
          val middleEnd = rotate(getLineEnd(connCircle, 0D), rot)
          val rightEnd = rotate(getLineEnd(connCircle, -dda), rot)
          Set(Line(leftStart, leftEnd), Line(middleStart, middleEnd), Line(rightStart, rightEnd))
        case MarkType.LINE =>
          val start = rotate(getLineEnd(original, 0D), rot)
          val end = rotate(getLineEnd(connCircle, 0D), rot)
          Set(Line(start, end))
        case MarkType.DOUBLE_LINE =>
          val leftStart = rotate(getLineEnd(original, da), rot)
          val rightStart = rotate(getLineEnd(original, -da), rot)
          val leftEnd = rotate(getLineEnd(connCircle, da), rot)
          val rightEnd = rotate(getLineEnd(connCircle, -da), rot)
          Set(Line(leftStart, leftEnd), Line(rightStart, rightEnd))
      }
    }
  }

  private def drawVowel(g2d: Graphics2D, vow: Vowel, sylCircle: Circle, isDouble: Boolean, rot: Double): Set[Line] = {
    val offset = (sylCircle.radius * vow.position.offset).intValue
    val original = sylCircle.center.addToY(offset)
    val center = rotate(original, rot)
    val rat = if (isDouble) { vow.position.doubleRatio } else { vow.position.ratio }
    val radius = (sylCircle.radius * rat).intValue
    val circle = Circle(center, radius)
    drawCircle(g2d, circle, false)
    if (isDouble) { Set.empty } else {
      def lineLength: Int = sylCircle.radius / 2
      if (vow.position.equals(VowelPosition.CENTER_IN)) {
        val from = rotate(original.addToY(-radius), rot)
        val to = rotate(original.addToY(-radius - lineLength), rot)
        Set(Line(from, to))
      } else if (vow.position.equals(VowelPosition.CENTER_OUT)) {
        val from = rotate(original.addToY(radius), rot)
        val to = rotate(original.addToY(radius + lineLength), rot)
        Set(Line(from, to))
      } else { Set.empty }
    }
  }

  private def drawPunctation(g2d: Graphics2D, pun: Punctation, rot: Double): Set[Line] = {
    Set.empty //TODO implement
  }

  private def getLineEnd(circle: Circle, angle: Double): Coord = {
    val x = circle.center.x - (Math.sin(angle) * circle.radius).intValue
    val y = circle.center.y - (Math.cos(angle) * circle.radius).intValue - HALF_LINE
    Coord(x, y)
  }

  private def getOffsetAndSize(circle: Circle, con: Consonant): (Double, Int) = {
    val isOpenOrFull = con.circleType.equals(CircleType.OPEN) || con.circleType.equals(CircleType.FULL)
    val offset = if (isOpenOrFull) { Math.toRadians(-90D) } else { Math.toRadians(-45D) }
    val size = (circle.radius * 0.05D).intValue
    (offset, size)
  }

  private def getDot(circle: Circle, angle: Double): Coord = {
    val fac = circle.radius * 0.9D
    val x = circle.center.x - (Math.sin(angle) * fac).intValue
    val y = circle.center.y - (Math.cos(angle) * fac).intValue
    Coord(x, y)
  }

  private def rotate(coord: Coord, angle: Double): Coord = rotate(coord, angle, wc.center)
  private def rotate(coord: Coord, angle: Double, center: Coord): Coord = {
    val xDiff = (coord.x - center.x)
    val yDiff = (coord.y - center.y)
    val cosA = Math.cos(Math.toRadians(angle))
    val sinA = Math.sin(Math.toRadians(angle))
    val x = center.x + (cosA * xDiff) - (sinA * yDiff)
    val y = center.y + (sinA * xDiff) + (cosA * yDiff)
    Coord(x.intValue, y.intValue)
  }

  private def drawConsonantPoint(g2d: Graphics2D, pos: Coord, size: Int): Unit = {
    val doubleSize = size * 2
    g2d.fill(new Ellipse2D.Double(pos.x - size, pos.y - size, doubleSize, doubleSize))
  }

  private def drawArc(g2d: Graphics2D, circle: Circle, s: Coord, e: Coord): Unit = {
    val arc = new Arc2D.Double(Arc2D.OPEN)
    arc.setFrame(circle.xStart, circle.yStart, circle.diameter, circle.diameter)
    arc.setAngles(s.x, s.y, e.x, e.y)
    g2d.setStroke(STROKE)
    g2d.draw(arc)
  }

  private def drawCircle(g2d: Graphics2D, circle: Circle, fill: Boolean): Unit = {
    val ellipse = new Ellipse2D.Double(circle.xStart, circle.yStart, circle.diameter, circle.diameter)
    if (!fill) { g2d.draw(ellipse) } else {
      val oldColor = g2d.getPaint
      g2d.setPaint(g2d.getBackground)
      g2d.fill(ellipse)
      g2d.setPaint(oldColor)
    }
  }

  private def drawLine(g2d: Graphics2D, s: Coord, e: Coord): Unit = g2d.drawLine(s.x, s.y, e.x, e.y)

  private def drawPoint(g2d: Graphics2D, pos: Coord, color: Color): Unit = {
    val oldColor = g2d.getPaint
    g2d.setPaint(color)
    g2d.fill(new Ellipse2D.Double(pos.x - 2D, pos.y - 2D, 4D, 4D))
    g2d.setPaint(oldColor)
  }

  @deprecated("Needs better implementation after sentence construction is completed.", "2013-09-17")
  private def drawConnections(g2d: Graphics2D, connectorPoints: List[Set[Coord]]): Unit = {
    val halfSize = connectorPoints.size / 2
    def makeConnector(c: Coord, i: Int): Connector = Connector(i, c, CalcUtil.calcDistance(c, wc.center))
    def makeConnectorSet(s: Set[Coord], i: Int): Set[Connector] = s.map(makeConnector(_, i))
    def getSwitchedIndex(i: Int): Int = (i + halfSize) % connectorPoints.size
    def switchIndex(c: Connector): Connector = c.copy(index = getSwitchedIndex(c.index))
    def isEven(i: Int): Boolean = i % 2 == 0
    def sortCounterClock(cs: Set[Coord]): Set[Coord] = cs.toList.sortBy(c => CalcUtil.calcAngle(wc.center, c)).toSet
    //val sorted = connectorPoints.map(_.toList.sortBy(c => (c.x, c.y)).toSet)
    val sorted = connectorPoints.map(sortCounterClock(_))
    val indexed = sorted.zipWithIndex
    val conns = indexed.map(si => makeConnectorSet(si._1, si._2))
    val flat = conns.flatten.sortBy(_.distanceToCenter).sortBy(_.index)
    val halfRot = flat.map(c => switchIndex(c)).sortBy(_.index)
    val zipped = flat.zip(halfRot)
    val lines = zipped.map(z => Connection(z._1.coord, z._2.coord))
    val even = if (!isEven(lines.size)) {
      val angle = CalcUtil.calcAngle(wc.center, lines.head.from)
      val rotEnd = rotate(Coord(wc.center.x, wc.center.y + wc.radius), 90D + angle)
      drawLine(g2d, lines.head.from, rotEnd)
      lines.tail
    } else { lines }
    /*
     * TODO find closest unconnected with different index.. 1. right, then left, then center
     * find closest unconnected with different index
     * - first tripple right
     * - then tripple left
     * - then dual right
     * - then dual left
     * - then center
     */
    //even.takeRight(halfSize + 1).foreach(c => drawLine(g2d, c.from, c.to))
    even.foreach(c => drawLine(g2d, c.from, c.to))
  }
}

package gallifreyan.util

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Font
import java.awt.Graphics2D
import java.awt.geom.Arc2D
import java.awt.geom.Ellipse2D

import gallifreyan.Size
import gallifreyan.engine.CircleType
import gallifreyan.engine.MarkType
import gallifreyan.engine.VowelPosition
import gallifreyan.engine.cases.Circle
import gallifreyan.engine.cases.Coord
import gallifreyan.engine.cases.Line
import gallifreyan.engine.characters.Consonant
import gallifreyan.engine.characters.Punctation
import gallifreyan.engine.characters.Vowel
import gallifreyan.engine.data.Sentence
import gallifreyan.engine.data.Syllable
import gallifreyan.engine.data.Character

object DrawUtil {
  val conceptMode = true
  val LINE_WIDTH = 2
  val HALF_LINE = LINE_WIDTH / 2
  val STROKE = new BasicStroke(LINE_WIDTH)
  val FONT = new Font(Font.MONOSPACED, Font.BOLD, 30)
  val CONN_MARK_SIZE = 10

  val l: List[Double] = List(0)

  def drawSentence(g2d: Graphics2D, sentence: Sentence, fg: Color, bg: Color, addText: Boolean): Unit = {
    //g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    //g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    g2d.setPaint(fg)
    g2d.setBackground(bg)
    g2d.setStroke(STROKE)
    g2d.clearRect(0, 0, Size.width, Size.height)
    drawCircle(g2d, wc, false)
    if (!sentence.v.isEmpty) {

      val word = sentence.v.head //FIXME implement

      val rot = 360D / word.v.size
      val rots = (1 to word.v.size).map(i => 360D - (rot * i)).reverse.toList
      val sizeRatio = CalcUtil.calcSizeRatio(word.v.size)
      val rotSent = word.v.zip(rots)
      val connectorLines: List[Set[Line]] = rotSent.map(si => drawSyllable(g2d, si._1, si._2, sizeRatio))
      markConnections(g2d, connectorLines)
      //val connectorPoints: List[Set[Coord]] = connectorLines.map(_.map(l => l.start))
      //drawConnections(g2d, connectorPoints)
    }
    if (addText) { g2d.drawString(sentence.mkString, 10, FONT.getSize) }
    g2d.dispose
  }

  private def markConnections(g2d: Graphics2D, lineSets: List[Set[Line]]): Unit = {
    lineSets.map(_.toList).flatten.foreach(l => drawLine(g2d, l.start, l.end))
  }

  private def drawSyllable(g2d: Graphics2D, syl: Syllable, rot: Double, sizeRatio: Double): Set[Line] = {
    def isDouble(i: Int, syl: Syllable): Boolean = i > 0 && syl.v(i) == syl.v(i - 1) //TODO don't access list by index
    val lastCon = syl.v.head match {
      case con: Consonant => Some(con)
      case _ => None
    }
    val connectors = syl.v.indices.map(i => drawCharacter(g2d, syl.v(i), makeSylCircle(syl, sizeRatio), lastCon, isDouble(i, syl), sizeRatio, rot))
    connectors.flatten.toSet
  }

  private def sc(): Circle = {
    val sentenceCenter = Coord(Size.width / 2, Size.height / 2)
    val sentenceRadius = (sentenceCenter.y * 0.9D).intValue
    Circle(sentenceCenter, sentenceRadius)
  }

  private def wc(): Circle = {
    val wordCenter = Coord(Size.width / 2, Size.height / 2)
    val wordRadius = (wordCenter.y * 0.7D).intValue
    Circle(wordCenter, wordRadius)
  }

  private def makeSylCircle(syl: Syllable, sizeRatio: Double): Circle = {
    syl.v(0) match {
      case con: Consonant => makeConCircle(con, wc, false, sizeRatio)
      case _ => makeConCircle(Consonant.TH, wc, false, sizeRatio)
      //Vowels in syllables without constants use the same circle as Th
    }
  }

  private def makeConCircle(con: Consonant, wordCircle: Circle, isDouble: Boolean, sizeRatio: Double): Circle = {
    val offset = (wordCircle.radius * con.circleType.offset).intValue
    val rat = if (isDouble) { con.circleType.doubleRatio } else { con.circleType.ratio }
    val radius = (wordCircle.radius * rat).intValue
    val fixedRadius = (radius * sizeRatio).intValue
    val fixedCenter = if (con.circleType.equals(CircleType.STRIKED)) {
      wordCircle.center.addToY(wordCircle.radius)
    } else {
      wordCircle.center.addToY(offset).addToY(radius - (fixedRadius))
    }
    Circle(fixedCenter, fixedRadius)
  }

  private def drawCharacter(g2d: Graphics2D, c: Character, sylCircle: Circle, lastCon: Option[Consonant],
    isDouble: Boolean, sizeRatio: Double, rot: Double): Set[Line] = {
    c match {
      case con: Consonant => drawConsonant(g2d, con, isDouble, sizeRatio, -rot)
      case vow: Vowel => drawVowel(g2d, vow, sylCircle, lastCon, isDouble, -rot)
      case pun: Punctation => drawPunctation(g2d, pun, -rot)
    }
  }

  private def drawConsonant(g2d: Graphics2D, con: Consonant, isDouble: Boolean, sizeRatio: Double, rot: Double): Set[Line] = {
    val original = makeConCircle(con, wc, isDouble, sizeRatio)
    def connCircle: Circle = original.addToRadius(CONN_MARK_SIZE)
    val circle = Circle(rotate(original.center, rot), original.radius)
    if (!con.circleType.isCrossing) { drawCircle(g2d, circle, false) } else {
      val (s, e) = CalcUtil.calcStartAndEnd(wc, original)
      drawArc(g2d, circle, rotate(e, rot), rotate(s, rot))
      if (!isDouble) { drawCircle(g2d, circle.addToRadius(-HALF_LINE), true) }
    }
    if (isDouble) { Set.empty } else {
      def da: Double = Math.toRadians(10D)
      def dda: Double = da * 2D
      con.markType match {
        case MarkType.NONE => Set.empty
        case MarkType.DOUBLE_DOT =>
          val (offset, size) = CalcUtil.calcOffsetAndSize(circle, con)
          drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset + da), rot), size)
          drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset - da), rot), size)
          Set.empty
        case MarkType.TRIPPLE_DOT =>
          val (offset, size) = CalcUtil.calcOffsetAndSize(circle, con)
          drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset + dda), rot), size)
          drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset), rot), size)
          drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset - dda), rot), size)
          Set.empty
        case MarkType.TRIPPLE_LINE =>
          val leftStart = rotate(CalcUtil.calcLineEnd(original, dda, HALF_LINE), rot)
          val middleStart = rotate(CalcUtil.calcLineEnd(original, 0D, HALF_LINE), rot)
          val rightStart = rotate(CalcUtil.calcLineEnd(original, -dda, HALF_LINE), rot)
          val leftEnd = rotate(CalcUtil.calcLineEnd(connCircle, dda, HALF_LINE), rot)
          val middleEnd = rotate(CalcUtil.calcLineEnd(connCircle, 0D, HALF_LINE), rot)
          val rightEnd = rotate(CalcUtil.calcLineEnd(connCircle, -dda, HALF_LINE), rot)
          Set(Line(leftStart, leftEnd), Line(middleStart, middleEnd), Line(rightStart, rightEnd))
        case MarkType.LINE =>
          val start = rotate(CalcUtil.calcLineEnd(original, 0D, HALF_LINE), rot)
          val end = rotate(CalcUtil.calcLineEnd(connCircle, 0D, HALF_LINE), rot)
          Set(Line(start, end))
        case MarkType.DOUBLE_LINE =>
          val leftStart = rotate(CalcUtil.calcLineEnd(original, da, HALF_LINE), rot)
          val rightStart = rotate(CalcUtil.calcLineEnd(original, -da, HALF_LINE), rot)
          val leftEnd = rotate(CalcUtil.calcLineEnd(connCircle, da, HALF_LINE), rot)
          val rightEnd = rotate(CalcUtil.calcLineEnd(connCircle, -da, HALF_LINE), rot)
          Set(Line(leftStart, leftEnd), Line(rightStart, rightEnd))
      }
    }
  }

  private def drawVowel(g2d: Graphics2D, vow: Vowel, sylCircle: Circle, lastCon: Option[Consonant], isDouble: Boolean, rot: Double): Set[Line] = {
    def offset: Int = (sylCircle.radius * vow.position.offset).intValue
    def halfOff: Int = offset / 2
    val original = if (vow.position.equals(VowelPosition.OUT)) {
      wc.center.addToY(wc.radius + halfOff)
    } else if (vow.position.equals(VowelPosition.IN)) {
      lastCon match {
        case Some(c) => sylCircle.center.addToY(offset)
        case _ => wc.center.addToY(wc.radius + halfOff)
      }
    } else {
      lastCon match {
        case Some(c) if c.circleType.equals(CircleType.HALF) => wc.center.addToY(wc.radius)
        case _ => sylCircle.center.addToY(offset)
      }
    }
    val center = rotate(original, rot)
    val rat = if (isDouble) { vow.position.doubleRatio } else { vow.position.ratio }
    val radius = lastCon match {
      case Some(c) if c.circleType.equals(CircleType.HALF) => (sylCircle.radius * rat * CircleType.openHalfRatio).intValue
      case _ => (sylCircle.radius * rat).intValue
    }
    val circle = Circle(center, radius)
    drawCircle(g2d, circle, false)
    if (isDouble) { Set.empty } else {
      def lineLength: Int = sylCircle.radius / 3
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

  private def drawLine(g2d: Graphics2D, s: Coord, e: Coord): Unit = {
    //This method is using polyline instead of line to allow better manipulation of the resulting SVG
    //g2d.drawLine(s.x, s.y, e.x, e.y)
    g2d.drawPolyline(List(s.x, e.x).toArray, List(s.y, e.y).toArray.toArray, 2)
  }

  private def drawPoint(g2d: Graphics2D, pos: Coord, color: Color): Unit = {
    val oldColor = g2d.getPaint
    g2d.setPaint(color)
    g2d.fill(new Ellipse2D.Double(pos.x - 2D, pos.y - 2D, 4D, 4D))
    g2d.setPaint(oldColor)
  }
}

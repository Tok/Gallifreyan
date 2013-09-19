package gallifreyan.util

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Font
import java.awt.Graphics2D
import java.awt.Polygon
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
import gallifreyan.engine.data.Character
import gallifreyan.engine.data.Sentence
import gallifreyan.engine.data.Syllable
import gallifreyan.engine.data.Word
import java.awt.Shape
import java.awt.Rectangle
import java.awt.Polygon

object DrawUtil {
  val conceptMode = true
  val LINE_WIDTH = 2
  val HALF_LINE = LINE_WIDTH / 2
  val STROKE = new BasicStroke(LINE_WIDTH)
  val FONT = new Font(Font.MONOSPACED, Font.BOLD, 30)
  val CONN_MARK_SIZE = 10

  type LineSets = List[Set[Line]]

  def drawSentence(g2d: Graphics2D, sentence: Sentence, fg: Color, bg: Color, addText: Boolean): Unit = {
    //g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    //g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    g2d.setPaint(fg)
    g2d.setBackground(bg)
    g2d.setStroke(STROKE)
    g2d.clearRect(0, 0, Size.width, Size.height)
    if (!sentence.v.isEmpty) {
      val connectorLines: LineSets = {
        if (sentence.isSingleWord) { drawSingleWord(g2d, sentence.v.head) }
        else {
          val sentSizeRatio = CalcUtil.calcSizeRatio(sentence.v.size)
          sentence.zipRots.map(z => drawWord(g2d, z._1, z._2, sentSizeRatio)).flatten
        }
      }
      markConnections(g2d, connectorLines)

      //TODO implement
      //val connectorPoints: List[Set[Coord]] = connectorLines.map(_.map(l => l.start))
      //drawConnections(g2d, connectorPoints)

      drawSentence(g2d, sentence)
      if (addText) { writeText(g2d, sentence) }
    }
    g2d.dispose
  }

  private def drawSingleWord(g2d: Graphics2D, word: Word): LineSets = {
    drawCircle(g2d, Word.circle)
    val wordSizeRatio = CalcUtil.calcSizeRatio(word.v.size)
    word.zipRots.map(z => drawSyllable(g2d, z._1, z._2, wordSizeRatio, Word.circle))
  }

  private def drawWord(g2d: Graphics2D, word: Word, sentRot: Double, sentRatio: Double): LineSets = {
    def wc: Circle = {
      val sc = Sentence.circle
      val offset = (sc.radius * 0.6D).intValue
      val radius = (sc.radius * 0.35D * sentRatio).intValue
      val center = rotate(sc.center.addToY(offset), sentRot, sc)
      Circle(center, radius)
    }
    drawCircle(g2d, wc)
    val wordSizeRatio = CalcUtil.calcSizeRatio(word.v.size)
    word.zipRots.map(z => drawSyllable(g2d, z._1, z._2, wordSizeRatio, wc))
  }

  private def makeSylCircle(syl: Syllable, sizeRatio: Double, wordCircle: Circle): Circle = {
    syl.v(0) match {
      case con: Consonant => makeConCircle(con, wordCircle, false, sizeRatio)
      case _ => makeConCircle(Consonant.TH, wordCircle, false, sizeRatio)
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

  private def writeText(g2d: Graphics2D, sentence: Sentence): Unit = g2d.drawString(sentence.mkString, 10, FONT.getSize)

  private def drawSentence(g2d: Graphics2D, sentence: Sentence): Unit = {
    if (!sentence.isSingleWord) {
      drawCircle(g2d, Sentence.circle)
      drawCircle(g2d, Sentence.circle.addToRadius(LINE_WIDTH * 7))
    }
  }

  private def markConnections(g2d: Graphics2D, lineSets: LineSets): Unit = {
    lineSets.map(_.toList).flatten.foreach(l => drawLine(g2d, l.start, l.end))
  }

  private def drawSyllable(g2d: Graphics2D, syl: Syllable, rot: Double, sizeRatio: Double, wc: Circle): Set[Line] = {
    def isDouble(i: Int, syl: Syllable): Boolean = i > 0 && syl.v(i) == syl.v(i - 1) //TODO don't access list by index
    val lastCon = syl.v.head match {
      case con: Consonant => Some(con)
      case _ => None
    }
    val sylCircle = makeSylCircle(syl, sizeRatio, wc)
    val connectors = syl.v.indices.map(i => drawCharacter(g2d, syl.v(i), sylCircle, lastCon, isDouble(i, syl), sizeRatio, rot, wc))
    connectors.flatten.toSet
  }

  private def drawCharacter(g2d: Graphics2D, c: Character, sylCircle: Circle, lastCon: Option[Consonant],
      isDouble: Boolean, sizeRatio: Double, rot: Double, wc: Circle): Set[Line] = {
    c match {
      case con: Consonant => drawConsonant(g2d, con, isDouble, sizeRatio, -rot, wc)
      case vow: Vowel => drawVowel(g2d, vow, sylCircle, lastCon, isDouble, -rot, wc)
      case pun: Punctation => drawPunctation(g2d, pun, -rot, wc)
    }
  }

  private def drawConsonant(g2d: Graphics2D, con: Consonant, isDouble: Boolean, sizeRatio: Double, rot: Double, wc: Circle): Set[Line] = {
    val original = makeConCircle(con, wc, isDouble, sizeRatio)
    def connCircle: Circle = original.addToRadius(CONN_MARK_SIZE)
    val circle = Circle(rotate(original.center, rot, wc), original.radius)
    if (!con.circleType.isCrossing) { drawCircle(g2d, circle) } else {
      val (s, e) = CalcUtil.calcStartAndEnd(wc, original)
      val rotE = rotate(e, rot, wc)
      val rotS = rotate(s, rot, wc)
      if (!isDouble) {
        con.circleType match {
          case CircleType.HALF => fillRect(g2d, circle.center, rotE, rotS)
          case _ => fillCircle(g2d, circle.addToRadius(-HALF_LINE))
        }
      }
      drawArc(g2d, circle, rotE, rotS)
    }
    if (isDouble) { Set.empty } else {
      def da: Double = Math.toRadians(10D)
      def dda: Double = da * 2D
      def daDot: Double = Math.toRadians(8D)
      def ddaDot: Double = daDot * 2D
      con.markType match {
        case MarkType.NONE => Set.empty
        case MarkType.DOUBLE_DOT =>
          val (offset, size) = CalcUtil.calcOffsetAndSize(circle, con)
          drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset + daDot), rot, wc), size)
          drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset - daDot), rot, wc), size)
          Set.empty
        case MarkType.TRIPPLE_DOT =>
          val (offset, size) = CalcUtil.calcOffsetAndSize(circle, con)
          drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset + ddaDot), rot, wc), size)
          drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset), rot, wc), size)
          drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset - ddaDot), rot, wc), size)
          Set.empty
        case MarkType.TRIPPLE_LINE =>
          val leftStart = rotate(CalcUtil.calcLineEnd(original, dda, HALF_LINE), rot, wc)
          val middleStart = rotate(CalcUtil.calcLineEnd(original, 0D, HALF_LINE), rot, wc)
          val rightStart = rotate(CalcUtil.calcLineEnd(original, -dda, HALF_LINE), rot, wc)
          val leftEnd = rotate(CalcUtil.calcLineEnd(connCircle, dda, HALF_LINE), rot, wc)
          val middleEnd = rotate(CalcUtil.calcLineEnd(connCircle, 0D, HALF_LINE), rot, wc)
          val rightEnd = rotate(CalcUtil.calcLineEnd(connCircle, -dda, HALF_LINE), rot, wc)
          Set(Line(leftStart, leftEnd), Line(middleStart, middleEnd), Line(rightStart, rightEnd))
        case MarkType.LINE =>
          val start = rotate(CalcUtil.calcLineEnd(original, 0D, HALF_LINE), rot, wc)
          val end = rotate(CalcUtil.calcLineEnd(connCircle, 0D, HALF_LINE), rot, wc)
          Set(Line(start, end))
        case MarkType.DOUBLE_LINE =>
          val leftStart = rotate(CalcUtil.calcLineEnd(original, da, HALF_LINE), rot, wc)
          val rightStart = rotate(CalcUtil.calcLineEnd(original, -da, HALF_LINE), rot, wc)
          val leftEnd = rotate(CalcUtil.calcLineEnd(connCircle, da, HALF_LINE), rot, wc)
          val rightEnd = rotate(CalcUtil.calcLineEnd(connCircle, -da, HALF_LINE), rot, wc)
          Set(Line(leftStart, leftEnd), Line(rightStart, rightEnd))
      }
    }
  }

  private def drawVowel(g2d: Graphics2D, vow: Vowel, sylCircle: Circle, lastCon: Option[Consonant], isDouble: Boolean, rot: Double, wc: Circle): Set[Line] = {
    def offset: Int = (sylCircle.radius * vow.position.offset).intValue
    def halfOff: Int = offset / 2
    val original = if (vow.position.equals(VowelPosition.OUT)) {
      lastCon match {
        case Some(c) if c.circleType.equals(CircleType.HALF) =>
          val off: Int = (offset * 0.5D * CircleType.FULL.ratio / CircleType.HALF.ratio).intValue
          wc.center.addToY(wc.radius + off)
        case _ => wc.center.addToY(wc.radius + halfOff)
      }
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
    val center = rotate(original, rot, wc)
    val rat = if (isDouble) { vow.position.doubleRatio } else { vow.position.ratio }
    val radius = lastCon match {
      case Some(c) if c.circleType.equals(CircleType.HALF) => (sylCircle.radius * rat * CircleType.openHalfRatio).intValue
      case _ => (sylCircle.radius * rat).intValue
    }
    val circle = Circle(center, radius)
    drawCircle(g2d, circle)
    if (isDouble) { Set.empty } else {
      def lineLength: Int = sylCircle.radius / 3
      if (vow.position.equals(VowelPosition.CENTER_IN)) {
        val from = rotate(original.addToY(-radius), rot, wc)
        val to = rotate(original.addToY(-radius - lineLength), rot, wc)
        Set(Line(from, to))
      } else if (vow.position.equals(VowelPosition.CENTER_OUT)) {
        val from = rotate(original.addToY(radius), rot, wc)
        val to = rotate(original.addToY(radius + lineLength), rot, wc)
        Set(Line(from, to))
      } else { Set.empty }
    }
  }

  private def drawPunctation(g2d: Graphics2D, pun: Punctation, rot: Double, wc: Circle): Set[Line] = {
    Set.empty //TODO implement
  }

  private def rotate(coord: Coord, angle: Double, cir: Circle): Coord = rotate(coord, angle, cir.center)
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

  private def fillRect(g2d: Graphics2D, center: Coord, p1: Coord, p2: Coord): Unit = {
    val p3 = rotate(p1, 180D, center)
    val p4 = rotate(p2, 180D, center)
    val x: Array[Int] = Array(p1.x, p2.x, p3.x, p4.x)
    val y: Array[Int] = Array(p1.y, p2.y, p3.y, p4.y)
    val triangle = new Polygon(x, y, 4)
    drawOrFill(g2d, triangle, true)
  }

  private def drawArc(g2d: Graphics2D, circle: Circle, s: Coord, e: Coord): Unit = {
    val arc = new Arc2D.Double(Arc2D.OPEN)
    arc.setFrame(circle.xStart, circle.yStart, circle.diameter, circle.diameter)
    arc.setAngles(s.x, s.y, e.x, e.y)
    g2d.setStroke(STROKE)
    drawOrFill(g2d, arc, false)
  }

  private def fillCircle(g2d: Graphics2D, circle: Circle): Unit = drawOrFillCircle(g2d, circle, true)
  private def drawCircle(g2d: Graphics2D, circle: Circle): Unit = drawOrFillCircle(g2d, circle, false)
  private def drawOrFillCircle(g2d: Graphics2D, circle: Circle, fill: Boolean): Unit = {
    val ellipse = new Ellipse2D.Double(circle.xStart, circle.yStart, circle.diameter, circle.diameter)
    drawOrFill(g2d, ellipse, fill)
  }

  private def drawOrFill(g2d: Graphics2D, shape: Shape, fill: Boolean): Unit = {
    if (!fill) { g2d.draw(shape) } else {
      val oldColor = g2d.getPaint
      g2d.setPaint(g2d.getBackground)
      g2d.fill(shape)
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

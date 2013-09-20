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

  def drawSentence(g2d: Graphics2D, sentence: Sentence, fg: Color, bg: Color, addText: Boolean, stubs: Boolean): Unit = {
    //g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    //g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    g2d.setPaint(fg)
    g2d.setBackground(bg)
    g2d.setStroke(STROKE)
    g2d.clearRect(0, 0, Size.width, Size.height)
    if (!sentence.v.isEmpty) {
      val connectorLines: LineSets = {
        if (sentence.isSingleWord) { drawSingleWord(g2d, sentence.v.head, stubs) }
        else {
          val sentSizeRatio = CalcUtil.calcSizeRatio(sentence.v.size)
          sentence.zipRots.map(z => drawWord(g2d, z._1, z._2, sentSizeRatio, stubs)).flatten
        }
      }
      if (stubs) {
        markStubs(g2d, connectorLines)
      } else {
        //println(connectorLines)
        val connectorPoints: List[Coord] = connectorLines.map(_.map(l => l.start)).flatten
        val connectorAngles: List[(Coord, Int)] = connectorPoints.map(c => (c, CalcUtil.calcAngle(Sentence.circle.center, c)))
        connectLines(g2d, connectorAngles)        
      }

      drawSentence(g2d, sentence)
      if (addText) { writeText(g2d, sentence) }
    }
    g2d.dispose
  }

  private def connectLines(g2d: Graphics2D, connectorAngles: List[(Coord, Int)]): Unit = {
    //println(connectorAngles)
  }

  private def markStubs(g2d: Graphics2D, lineSets: LineSets): Unit = {
    lineSets.map(_.toList).flatten.foreach(l => drawLine(g2d, l.start, l.end))
  }

  private def drawSingleWord(g2d: Graphics2D, word: Word, stubs: Boolean): LineSets = {
    drawCircle(g2d, Word.circle)
    val wordSizeRatio = CalcUtil.calcSizeRatio(word.v.size)
    val result = word.zipRots.map(z => drawSyllable(g2d, z._1, z._2, wordSizeRatio, Word.circle, stubs, true))
    word.zipRots.map(z => drawSyllable(g2d, z._1, z._2, wordSizeRatio, Word.circle, stubs, false))
    result
  }

  private def drawWord(g2d: Graphics2D, word: Word, sentRot: Double, sentRatio: Double, stubs: Boolean): LineSets = {
    def wc: Circle = {
      val sc = Sentence.circle
      val offset = (sc.radius * 0.6D).intValue
      val radius = (sc.radius * 0.35D * sentRatio).intValue
      val center = rotate(sc.center.addToY(offset), -sentRot, sc)
      Circle(center, radius)
    }
    drawCircle(g2d, wc)
    val wordSizeRatio = CalcUtil.calcSizeRatio(word.v.size)
    val result = word.zipRots.map(z => drawSyllable(g2d, z._1, z._2, wordSizeRatio, wc, stubs, true))
    word.zipRots.map(z => drawSyllable(g2d, z._1, z._2, wordSizeRatio, wc, stubs, false))
    result
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
    val outerCircle = Sentence.circle.addToRadius(LINE_WIDTH * 7)
    val divCircle = Sentence.circle.addToRadius((Sentence.circle.radius * 0.5D).intValue)
    def drawDivot(angle: Double, op: Option[Punctation]): Unit = {
      val center = Coord(divCircle.center.x, divCircle.center.y + divCircle.radius)
      val radius = Sentence.circle.radius * CalcUtil.calcSizeRatio(sentence.v.size + 1)
      val circle = Circle(center, radius.intValue)
      val (s, e) = CalcUtil.calcStartAndEnd(Sentence.circle, circle)
      val rotE = rotate(e, angle, Sentence.circle.center)
      val rotS = rotate(s, angle, Sentence.circle.center)
      val rotatedCircle = Circle(rotate(center, angle, Sentence.circle.center), radius.intValue)
      fillRect(g2d, rotatedCircle.center, rotE, rotS)
      drawArc(g2d, rotatedCircle, rotE, rotS)
      op.foreach(pun => drawPunctation(g2d, pun, rotatedCircle, outerCircle))
    }
    def drawDivots: Unit = {
      def getPunOption(c: Character): Option[Punctation] = {
        if (c.isInstanceOf[Punctation]) { Some(c.asInstanceOf[Punctation]) } else { None }
      }
      val pun = sentence.v.map(_.v.last).map(syl => getPunOption(syl.v.last))
      val angle = sentence.rots(1) / 2
      val angles = sentence.rots.map(r => (r * -1) - angle)
      val zipped = angles.zip(pun)
      zipped.foreach(tup => drawDivot(tup._1, tup._2))
    }
    if (!sentence.isSingleWord) {
      drawCircle(g2d, Sentence.circle)
      drawDivots
      drawCircle(g2d, outerCircle)
    }
  }

  private def drawPunctation(g2d: Graphics2D, pun: Punctation, cir: Circle, outer: Circle): Unit = {
    def close: Coord = cir.calcClosestTo(Sentence.circle.center)
    pun match {
      case Punctation.DOT =>
        drawCircle(g2d, Circle(close, 20))
      case Punctation.QUESTION =>
        val first = rotate(cir.moveFromCenter(close, 0.9D), -10D, cir.center)
        val second = rotate(cir.moveFromCenter(close, 0.9D), 10D, cir.center)
        drawConsonantPoint(g2d, first, 5)
        drawConsonantPoint(g2d, second, 5)
      case Punctation.EXCLAIM =>
        val first = rotate(cir.moveFromCenter(close, 0.9D), -15D, cir.center)
        val middle = cir.moveFromCenter(close, 0.9D)
        val second = rotate(cir.moveFromCenter(close, 0.9D), 15D, cir.center)
        drawConsonantPoint(g2d, first, 5)
        drawConsonantPoint(g2d, middle, 5)
        drawConsonantPoint(g2d, second, 5)
      case Punctation.DOUBLEQUOTE =>
        drawLine(g2d, close, outer.calcClosestTo(close))
      case Punctation.QUOTE =>
        val first = rotate(close, -10D, cir.center)
        val second = rotate(close, 10D, cir.center)
        drawLine(g2d, first, rotate(outer.calcClosestTo(close), 10D, outer))
        drawLine(g2d, second, rotate(outer.calcClosestTo(close), -10D, outer))
      case Punctation.HYPHEN =>
        val first = rotate(close, -15D, cir.center)
        val second = rotate(close, 15D, cir.center)
        drawLine(g2d, first, rotate(outer.calcClosestTo(close), 10D, outer))
        drawLine(g2d, close, outer.calcClosestTo(close))
        drawLine(g2d, second, rotate(outer.calcClosestTo(close), -10D, outer))
      case Punctation.COMMA =>
        drawConsonantPoint(g2d, close, 20)
      case Punctation.SEMICOLON =>
        drawConsonantPoint(g2d, cir.moveFromCenter(close, 0.9D), 5)
      case Punctation.COLON =>
        drawCircle(g2d, Circle(close, 20))
        drawCircle(g2d, Circle(close, 16))
    }
  }

  private def drawSyllable(g2d: Graphics2D, syl: Syllable, rot: Double, sizeRatio: Double,
    wc: Circle, stubs: Boolean, linesOnly: Boolean): Set[Line] = {
    def isDouble(i: Int, syl: Syllable): Boolean = i > 0 && syl.v(i) == syl.v(i - 1) //TODO don't access list by index
    val lastCon = syl.v.head match {
      case con: Consonant => Some(con)
      case _ => None
    }
    val sylCircle = makeSylCircle(syl, sizeRatio, wc)
    val connectors = syl.v.indices.map(i => drawCharacter(g2d, syl.v(i), sylCircle, lastCon, isDouble(i, syl), sizeRatio, rot, wc, stubs, linesOnly))
    connectors.flatten.toSet
  }

  private def drawCharacter(g2d: Graphics2D, c: Character, sylCircle: Circle, lastCon: Option[Consonant],
    isDouble: Boolean, sizeRatio: Double, rot: Double, wc: Circle, stubs: Boolean, linesOnly: Boolean): Set[Line] = {
    c match {
      case con: Consonant => drawConsonant(g2d, con, isDouble, sizeRatio, -rot, wc, stubs, linesOnly)
      case vow: Vowel =>
        if (!linesOnly) { drawVowel(g2d, vow, sylCircle, lastCon, isDouble, -rot, wc, stubs) }
        else { Set.empty }
      case pun: Punctation => Set.empty
    }
  }

  private def drawConsonant(g2d: Graphics2D, con: Consonant, isDouble: Boolean, sizeRatio: Double,
    rot: Double, wc: Circle, stubs: Boolean, linesOnly: Boolean): Set[Line] = {
    val original = makeConCircle(con, wc, isDouble, sizeRatio)
    def connCircle: Circle = original.addToRadius(CONN_MARK_SIZE)
    val circle = Circle(rotate(original.center, rot, wc), original.radius)
    if (!linesOnly) {
      if (!con.circleType.isCrossing) { drawCircle(g2d, circle) }
      else {
        val (s, e) = CalcUtil.calcStartAndEnd(wc, original)
        val rotE = rotate(e, rot, wc)
        val rotS = rotate(s, rot, wc)
        if (!isDouble) {
          con.circleType match {
            case CircleType.HALF if stubs => fillRect(g2d, circle.center, rotE, rotS)
            case _ => fillCircle(g2d, circle.addToRadius(-HALF_LINE))
          }
        }
        drawArc(g2d, circle, rotE, rotS)
      }
    }
    if (isDouble) { Set.empty } else {
      def da: Double = Math.toRadians(10D)
      def dda: Double = da * 2D
      def daDot: Double = Math.toRadians(8D)
      def ddaDot: Double = daDot * 2D
      con.markType match {
        case MarkType.NONE => Set.empty
        case MarkType.DOUBLE_DOT =>
          if (!linesOnly) {
            val (offset, size) = CalcUtil.calcOffsetAndSize(circle, con)
            drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset + daDot), rot, wc), size)
            drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset - daDot), rot, wc), size)
          }
          Set.empty
        case MarkType.TRIPPLE_DOT =>
          if (!linesOnly) {
            val (offset, size) = CalcUtil.calcOffsetAndSize(circle, con)
            drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset + ddaDot), rot, wc), size)
            drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset), rot, wc), size)
            drawConsonantPoint(g2d, rotate(CalcUtil.calcDot(original, offset - ddaDot), rot, wc), size)
          }
          Set.empty
        case MarkType.TRIPPLE_LINE =>
          if (linesOnly) {
            val leftStart = rotate(CalcUtil.calcLineEnd(original, dda, HALF_LINE), rot, wc)
            val middleStart = rotate(CalcUtil.calcLineEnd(original, 0D, HALF_LINE), rot, wc)
            val rightStart = rotate(CalcUtil.calcLineEnd(original, -dda, HALF_LINE), rot, wc)
            val leftEnd = rotate(CalcUtil.calcLineEnd(connCircle, dda, HALF_LINE), rot, wc)
            val middleEnd = rotate(CalcUtil.calcLineEnd(connCircle, 0D, HALF_LINE), rot, wc)
            val rightEnd = rotate(CalcUtil.calcLineEnd(connCircle, -dda, HALF_LINE), rot, wc)
            if (stubs) {
              Set(Line(leftStart, leftEnd), Line(middleStart, middleEnd), Line(rightStart, rightEnd))
            } else {
              val realLeftEnd = rotate(wc.calcFarthestFrom(rightEnd), 5D, wc.center)
              val realMiddleEnd = wc.calcFarthestFrom(middleEnd)
              val realRightEnd = rotate(wc.calcFarthestFrom(leftEnd), -5D, wc.center)
              drawLine(g2d, leftStart, realLeftEnd)
              drawLine(g2d, middleStart, realMiddleEnd)
              drawLine(g2d, rightStart, realRightEnd)
              Set(Line(leftStart, realLeftEnd), Line(middleStart, realMiddleEnd), Line(rightStart, realRightEnd))
            }
          } else { Set.empty }
        case MarkType.LINE =>
          if (linesOnly) {
            val start = rotate(CalcUtil.calcLineEnd(original, 0D, HALF_LINE), rot, wc)
            val end = rotate(CalcUtil.calcLineEnd(connCircle, 0D, HALF_LINE), rot, wc)
            if (stubs) {
              Set(Line(start, end))
            } else {
              val realEnd = wc.calcFarthestFrom(end)
              drawLine(g2d, start, realEnd)
              Set(Line(start, realEnd))
            }
          } else { Set.empty }
        case MarkType.DOUBLE_LINE =>
          if (linesOnly) {
            val leftStart = rotate(CalcUtil.calcLineEnd(original, da, HALF_LINE), rot, wc)
            val rightStart = rotate(CalcUtil.calcLineEnd(original, -da, HALF_LINE), rot, wc)
            val leftEnd = rotate(CalcUtil.calcLineEnd(connCircle, da, HALF_LINE), rot, wc)
            val rightEnd = rotate(CalcUtil.calcLineEnd(connCircle, -da, HALF_LINE), rot, wc)
            if (stubs) {
              Set(Line(leftStart, leftEnd), Line(rightStart, rightEnd))
            } else {
              val realLeftEnd = rotate(wc.calcFarthestFrom(rightEnd), 5D, wc.center)
              val realRightEnd = rotate(wc.calcFarthestFrom(leftEnd), -5D, wc.center)
              drawLine(g2d, leftStart, realLeftEnd)
              drawLine(g2d, rightStart, realRightEnd)
              Set(Line(leftStart, realLeftEnd), Line(rightStart, realRightEnd))
            }
          } else { Set.empty }
      }
    }
  }

  private def drawVowel(g2d: Graphics2D, vow: Vowel, sylCircle: Circle, lastCon: Option[Consonant],
    isDouble: Boolean, rot: Double, wc: Circle, stubs: Boolean): Set[Line] = {
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
    def getResult(from: Coord, to: Coord): Set[Line] = {
      if (stubs) {
        drawLine(g2d, from, to)
        Set(Line(from, to))
      } else {
        val realTo = if (vow.position.equals(VowelPosition.CENTER_IN)) { wc.calcFarthestFrom(from) }
        else if (vow.position.equals(VowelPosition.CENTER_OUT)) { circle.moveFromCenter(from, 7D) }
        else { to }
        drawLine(g2d, from, realTo)
        Set(Line(from, realTo))
      }
    }
    if (isDouble) { Set.empty } else {
      def lineLength: Int = sylCircle.radius / 3
      if (vow.position.equals(VowelPosition.CENTER_IN)) {
        val from = rotate(original.addToY(-radius), rot, wc)
        val to = rotate(original.addToY(-radius - lineLength), rot, wc)
        getResult(from, to)
      } else if (vow.position.equals(VowelPosition.CENTER_OUT)) {
        val from = rotate(original.addToY(radius), rot, wc)
        val to = rotate(original.addToY(radius + lineLength), rot, wc)
        getResult(from, to)
      } else { Set.empty }
    }
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

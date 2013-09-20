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
  val CONN_MARK_SIZE = 14

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
      drawSentence(g2d, sentence)
      if (addText) { writeText(g2d, sentence) }
      if (stubs) {
        markStubs(g2d, connectorLines)
      } else {        
        connectLinesToSc(g2d, connectorLines, sentence)
        //connectLines(g2d, connectorLines)
      }
    }
    g2d.dispose
  }

  private def connectLinesToSc(g2d: Graphics2D, connectorLines: LineSets, sentence: Sentence): Unit = {
    def selectCircle: Circle = if (sentence.isSingleWord) { Sentence.circle } else { Sentence.outer(LINE_WIDTH) }
    def calcInter(line: Line): Coord = CalcUtil.calcIntersection(line.start, line.end, selectCircle) 
    connectorLines.flatten.foreach(line => drawLine(g2d, line.end, calcInter(line)))
  }

  private def connectLines(g2d: Graphics2D, connectorLines: LineSets): Unit = {
    val orb = 40
    val cent = Sentence.circle.center
    def pointsToCenter(from: Coord, to: Coord): Boolean = {
      println("from:" + Math.abs(CalcUtil.calcDistance(from, cent)))
      println("to:" + Math.abs(CalcUtil.calcDistance(to, cent)))
      println(Math.abs(CalcUtil.calcDistance(from, cent)) > Math.abs(CalcUtil.calcDistance(to, cent)))
      Math.abs(CalcUtil.calcDistance(from, cent)) > Math.abs(CalcUtil.calcDistance(to, cent))
    }

    println("connectorLines: " + connectorLines)
    val pointToCenter: List[Line] = connectorLines.flatten.filter(line => pointsToCenter(line.start, line.end))
    println("pointToCenter: " + pointToCenter)
    val connectorPoints: List[Coord] = pointToCenter.map(l => l.start)
    println("connectorPoints: " + connectorPoints)
    val connectorAngles: List[(Coord, Int)] = connectorPoints.map(c => (c, CalcUtil.calcAngle(c, cent)))
    println("connectorAngles: " + connectorAngles)
    val modded: List[(Coord, Int)] = connectorAngles.map(tup => (tup._1, (tup._2 + 360) % 360))
    println("modded: " + modded)
    val byDist: List[(Coord, Int)] = modded.sortBy(tup => CalcUtil.calcDistance(Sentence.circle.center, tup._1))
    println("byDist: " + byDist)

    val pairs: List[(Coord, Coord)] = for {
      tup1: (Coord, Int) <- byDist
      tup2: (Coord, Int) <- byDist
    } yield {
      val angleDiff = tup1._2 - tup2._2
      if (angleDiff - 180 >= -orb && angleDiff - 180 <= orb) {
        (tup1._1, tup2._1)
      } else {
        (Coord(0, 0), Coord(0, 0))
      }
    }
    val filtered = pairs.filterNot(_._1 == Coord(0, 0))

    println("filtered: " + filtered)
    println("-----------------")

    filtered.foreach(pair => drawLine(g2d, pair._1, pair._2))
  }

  private def markStubs(g2d: Graphics2D, lineSets: LineSets): Unit = {
    lineSets.map(_.toList).flatten.foreach(l => drawLine(g2d, l.start, l.end))
  }

  private def drawSingleWord(g2d: Graphics2D, word: Word, stubs: Boolean): LineSets = {
    drawCircle(g2d, Word.circle)
    val wordSizeRatio = CalcUtil.calcSizeRatio(word.v.size)
    word.zipRots.map(z => drawSyllable(g2d, z._1, z._2, wordSizeRatio, Word.circle, stubs, true))
    word.zipRots.map(z => drawSyllable(g2d, z._1, z._2, wordSizeRatio, Word.circle, stubs, false))
  }

  private def drawWord(g2d: Graphics2D, word: Word, sentRot: Double, sentRatio: Double, stubs: Boolean): LineSets = {
    def wc: Circle = {
      val sc = Sentence.circle
      val offset = (sc.radius * 0.6D).intValue
      val radius = (sc.radius * 0.35D * sentRatio).intValue
      val center = CalcUtil.rotate(sc.center.addToY(offset), -sentRot, sc.center)
      Circle(center, radius)
    }
    drawCircle(g2d, wc)
    val wordSizeRatio = CalcUtil.calcSizeRatio(word.v.size)
    word.zipRots.map(z => drawSyllable(g2d, z._1, z._2, wordSizeRatio, wc, stubs, true))
    word.zipRots.map(z => drawSyllable(g2d, z._1, z._2, wordSizeRatio, wc, stubs, false))
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
    val divCircle = Sentence.circle.addToRadius((Sentence.circle.radius * 0.5D).intValue)
    def drawDivot(angle: Double, op: Option[Punctation]): Unit = {
      val center = Coord(divCircle.center.x, divCircle.center.y + divCircle.radius)
      val radius = Sentence.circle.radius * CalcUtil.calcSizeRatio(sentence.v.size + 1)
      val circle = Circle(center, radius.intValue)
      val (s, e) = CalcUtil.calcStartAndEnd(Sentence.circle, circle)
      val rotE = CalcUtil.rotate(e, angle, Sentence.circle.center)
      val rotS = CalcUtil.rotate(s, angle, Sentence.circle.center)
      val rotatedCircle = Circle(CalcUtil.rotate(center, angle, Sentence.circle.center), radius.intValue)
      fillRect(g2d, rotatedCircle.center, rotE, rotS)
      drawArc(g2d, rotatedCircle, rotE, rotS)
      op.foreach(pun => drawPunctation(g2d, pun, rotatedCircle, Sentence.outer(LINE_WIDTH)))
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
      drawCircle(g2d, Sentence.outer(LINE_WIDTH))
    }
  }

  private def drawPunctation(g2d: Graphics2D, pun: Punctation, cir: Circle, outer: Circle): Unit = {
    def close: Coord = cir.calcClosestTo(Sentence.circle.center)
    pun match {
      case Punctation.DOT =>
        drawCircle(g2d, Circle(close, 20))
      case Punctation.QUESTION =>
        val first = CalcUtil.rotate(cir.moveFromCenter(close, 0.9D), -10D, cir.center)
        val second = CalcUtil.rotate(cir.moveFromCenter(close, 0.9D), 10D, cir.center)
        drawConsonantPoint(g2d, first, 5)
        drawConsonantPoint(g2d, second, 5)
      case Punctation.EXCLAIM =>
        val first = CalcUtil.rotate(cir.moveFromCenter(close, 0.9D), -15D, cir.center)
        val middle = cir.moveFromCenter(close, 0.9D)
        val second = CalcUtil.rotate(cir.moveFromCenter(close, 0.9D), 15D, cir.center)
        drawConsonantPoint(g2d, first, 5)
        drawConsonantPoint(g2d, middle, 5)
        drawConsonantPoint(g2d, second, 5)
      case Punctation.DOUBLEQUOTE =>
        drawLine(g2d, close, outer.calcClosestTo(close))
      case Punctation.QUOTE =>
        val first = CalcUtil.rotate(close, -10D, cir.center)
        val second = CalcUtil.rotate(close, 10D, cir.center)
        drawLine(g2d, first, CalcUtil.rotate(outer.calcClosestTo(close), 10D, outer.center))
        drawLine(g2d, second, CalcUtil.rotate(outer.calcClosestTo(close), -10D, outer.center))
      case Punctation.HYPHEN =>
        val first = CalcUtil.rotate(close, -15D, cir.center)
        val second = CalcUtil.rotate(close, 15D, cir.center)
        drawLine(g2d, first, CalcUtil.rotate(outer.calcClosestTo(close), 10D, outer.center))
        drawLine(g2d, close, outer.calcClosestTo(close))
        drawLine(g2d, second, CalcUtil.rotate(outer.calcClosestTo(close), -10D, outer.center))
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
    val circle = Circle(CalcUtil.rotate(original.center, rot, wc.center), original.radius)
    if (!linesOnly) {
      if (!con.circleType.isCrossing) { drawCircle(g2d, circle) }
      else {
        val (s, e) = CalcUtil.calcStartAndEnd(wc, original)
        val rotE = CalcUtil.rotate(e, rot, wc.center)
        val rotS = CalcUtil.rotate(s, rot, wc.center)
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
            drawConsonantPoint(g2d, CalcUtil.rotate(CalcUtil.calcDot(original, offset + daDot), rot, wc.center), size)
            drawConsonantPoint(g2d, CalcUtil.rotate(CalcUtil.calcDot(original, offset - daDot), rot, wc.center), size)
          }
          Set.empty
        case MarkType.TRIPPLE_DOT =>
          if (!linesOnly) {
            val (offset, size) = CalcUtil.calcOffsetAndSize(circle, con)
            drawConsonantPoint(g2d, CalcUtil.rotate(CalcUtil.calcDot(original, offset + ddaDot), rot, wc.center), size)
            drawConsonantPoint(g2d, CalcUtil.rotate(CalcUtil.calcDot(original, offset), rot, wc.center), size)
            drawConsonantPoint(g2d, CalcUtil.rotate(CalcUtil.calcDot(original, offset - ddaDot), rot, wc.center), size)
          }
          Set.empty
        case MarkType.TRIPPLE_LINE =>
          if (linesOnly) {
            val leftStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, dda, HALF_LINE), rot, wc.center)
            val middleStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, 0D, HALF_LINE), rot, wc.center)
            val rightStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, -dda, HALF_LINE), rot, wc.center)
            val leftEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, dda, HALF_LINE), rot, wc.center)
            val middleEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, 0D, HALF_LINE), rot, wc.center)
            val rightEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, -dda, HALF_LINE), rot, wc.center)
            if (stubs) {
              Set(Line(leftStart, leftEnd), Line(middleStart, middleEnd), Line(rightStart, rightEnd))
            } else {
              Set(Line(circle.center, leftStart), Line(circle.center, middleStart), Line(circle.center, rightStart))
            }
          } else { Set.empty }
        case MarkType.LINE =>
          if (linesOnly) {
            val start = CalcUtil.rotate(CalcUtil.calcLineEnd(original, 0D, HALF_LINE), rot, wc.center)
            val end = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, 0D, HALF_LINE), rot, wc.center)
            if (stubs) {
              Set(Line(start, end))
            } else {
              Set(Line(circle.center, start))
            }
          } else { Set.empty }
        case MarkType.DOUBLE_LINE =>
          if (linesOnly) {
            val leftStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, da, HALF_LINE), rot, wc.center)
            val rightStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, -da, HALF_LINE), rot, wc.center)
            val leftEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, da, HALF_LINE), rot, wc.center)
            val rightEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, -da, HALF_LINE), rot, wc.center)
            if (stubs) {
              Set(Line(leftStart, leftEnd), Line(rightStart, rightEnd))
            } else {
              Set(Line(circle.center, leftStart), Line(circle.center, rightStart))
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
    val center = CalcUtil.rotate(original, rot, wc.center)
    val rat = if (isDouble) { vow.position.doubleRatio } else { vow.position.ratio }
    val radius = lastCon match {
      case Some(c) if c.circleType.equals(CircleType.HALF) => (sylCircle.radius * rat * CircleType.openHalfRatio).intValue
      case _ => (sylCircle.radius * rat).intValue
    }
    val circle = Circle(center, radius)
    drawCircle(g2d, circle)
    def getResult(from: Coord, to: Coord): Set[Line] = {
      if (stubs) {
        Set(Line(from, to))
      } else {
        Set(Line(circle.center, from))
      }
    }
    if (isDouble) { Set.empty } else {
      def lineLength: Int = sylCircle.radius / 3
      if (vow.position.equals(VowelPosition.CENTER_IN)) {
        val from = CalcUtil.rotate(original.addToY(-radius), rot, wc.center)
        val to = CalcUtil.rotate(original.addToY(-radius - lineLength), rot, wc.center)
        getResult(from, to)
      } else if (vow.position.equals(VowelPosition.CENTER_OUT)) {
        val from = CalcUtil.rotate(original.addToY(radius), rot, wc.center)
        val to = CalcUtil.rotate(original.addToY(radius + lineLength), rot, wc.center)
        getResult(from, to)
      } else { Set.empty }
    }
  }

  private def drawConsonantPoint(g2d: Graphics2D, pos: Coord, size: Int): Unit = {
    val doubleSize = size * 2
    g2d.fill(new Ellipse2D.Double(pos.x - size, pos.y - size, doubleSize, doubleSize))
  }

  private def fillRect(g2d: Graphics2D, center: Coord, p1: Coord, p2: Coord): Unit = {
    val p3 = CalcUtil.rotate(p1, 180D, center)
    val p4 = CalcUtil.rotate(p2, 180D, center)
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

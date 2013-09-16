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

object DrawUtil {
  val conceptMode = true
  
  val LINE_WIDTH = 2
  val HALF_LINE = LINE_WIDTH / 2
  val STROKE = new BasicStroke(LINE_WIDTH)
  val FONT = new Font(Font.MONOSPACED, Font.BOLD, 30)

  def drawSentence(g2d: Graphics2D, sent: Sentence, fg: Color, bg: Color, addText: Boolean): Unit = {
    //g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    //g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    g2d.setPaint(fg)
    g2d.setBackground(bg)
    g2d.setStroke(STROKE)
    g2d.clearRect(0, 0, Size.width, Size.height)
    val wc = getWordCircle
    drawCircle(g2d, wc, false)
    if (!sent.isEmpty) {
      val rot = 360D / sent.size
      val rots = (1 to sent.size).map(rot * _).reverse.toList
      val sizeRatio = CalcUtil.getSizeRatio(sent.size)
      val connectors: List[Set[Coord]] = sent.map(drawSyllable(g2d, _, rot, sizeRatio))
 
      g2d.rotate(Math.toRadians(rot * sent.size))

      val zipped: List[(Set[Coord], Double)] = connectors.zip(rots)

      def fixConns(coords: Set[Coord], rot: Double): Set[Coord] = coords.map(getRotatedDot(_, wc.center, rot))
      val fixedConns: List[Set[Coord]] = zipped.map(cr => fixConns(cr._1, cr._2))
      val flatConns: Set[Coord] = fixedConns.flatten.toSet

      flatConns.foreach(highlightLineConnector(g2d, _))
    }
    if (addText) {
      g2d.drawString(sent.map(_.mkString).mkString, 10, FONT.getSize)
    }
    g2d.dispose
  }

  private def drawSyllable(g2d: Graphics2D, syl: Syllable, rot: Double, sizeRatio: Double): Set[Coord] = {
    def isDouble(i: Int, syl: Syllable): Boolean = i > 0 && syl(i) == syl(i - 1)
    syl.indices.foreach(i => drawCharacter(g2d, syl(i), getSyllableCircle(syl, sizeRatio), isDouble(i, syl), sizeRatio))
    val connectors = syl.indices.map(i => drawCharacter(g2d, syl(i), getSyllableCircle(syl, sizeRatio), isDouble(i, syl), sizeRatio))
    g2d.rotate(Math.toRadians(-rot), getWordCircle.center.x, getWordCircle.center.y)
    connectors.flatten.toSet
  }

  private def getWordCircle(): Circle = {
    val wordCenter = Coord(Size.width / 2, Size.height / 2)
    val wordRadius = (wordCenter.y * 0.7D).intValue
    Circle(wordCenter, wordRadius)
  }

  private def getSyllableCircle(syl: Syllable, sizeRatio: Double): Circle = {
    syl(0) match {
      case con: Consonant => getConsonantCircle(con, getWordCircle, false, sizeRatio)
      case _ => getConsonantCircle(Consonant.TH, getWordCircle, false, sizeRatio)
      //Vowels in syllables without constants use the same circle as Th
    }
  }

  private def getConsonantCircle(con: Consonant, wordCircle: Circle, isDouble: Boolean, sizeRatio: Double): Circle = {
    val offset = (wordCircle.radius * con.circleType.offset).intValue
    val center = Coord(wordCircle.center.x, wordCircle.center.y + offset)
    val rat = if (isDouble) { con.circleType.doubleRatio } else { con.circleType.ratio }
    val radius = (wordCircle.radius * rat).intValue
    val fixedRadius = (radius * sizeRatio).intValue
    val fixedCenter = Coord(center.x, center.y + radius - (fixedRadius))
    Circle(fixedCenter, fixedRadius)
  }

  private def drawCharacter(g2d: Graphics2D, c: Character, sylCircle: Circle, isDouble: Boolean, sizeRatio: Double): Set[Coord] = {
    c match {
      case con: Consonant => drawConsonant(g2d, con, isDouble, sizeRatio)
      case vow: Vowel => drawVowel(g2d, vow, sylCircle, isDouble)
      case pun: Punctation => drawPunctation(g2d, pun)
    }
  }

  private def drawConsonant(g2d: Graphics2D, con: Consonant, isDouble: Boolean, sizeRatio: Double): Set[Coord] = {
    val wc = getWordCircle
    val circle = getConsonantCircle(con, wc, isDouble, sizeRatio)
    val (c1, c2) = CalcUtil.calcStartAndEnd(wc, circle)
    val isCrossing = con.circleType.equals(CircleType.OPEN) || con.circleType.equals(CircleType.HALF)
    if (isCrossing) {
      val start = CalcUtil.calcAngle(circle.center, c1)
      val end = CalcUtil.calcAngle(circle.center, c2)
      drawArc(g2d, circle, start, end, con.circleType)
      if (!isDouble) {
        drawCircle(g2d, circle.copy(radius = circle.radius - (LINE_WIDTH / 2)), true)
      }
    } else {
      drawCircle(g2d, circle, false)
    }
    val connectors: Set[Coord] = if (!isDouble) {
      val da = Math.toRadians(10D)
      con.markType match {
        case MarkType.NONE => Set.empty
        case MarkType.DOUBLE_DOT =>
          val (offset, size) = getOffsetAndSize(circle, con)
          drawConsonantPoint(g2d, getDot(circle, offset + da), size)
          drawConsonantPoint(g2d, getDot(circle, offset - da), size)
          Set.empty
        case MarkType.TRIPPLE_DOT =>
          val (offset, size) = getOffsetAndSize(circle, con)
          drawConsonantPoint(g2d, getDot(circle, offset + (da * 2D)), size)
          drawConsonantPoint(g2d, getDot(circle, offset), size)
          drawConsonantPoint(g2d, getDot(circle, offset - (da * 2D)), size)
          Set.empty
        case MarkType.TRIPPLE_LINE =>
          val angle = da * 2D
          drawLine(g2d, getLineEnd(circle, angle), getLineEnd(wc, angle))
          drawLine(g2d, getLineEnd(circle, 0D), getLineEnd(wc, 0D))
          drawLine(g2d, getLineEnd(circle, -angle), getLineEnd(wc, -angle))
          Set(getLineEnd(circle, angle), getLineEnd(circle, 0D), getLineEnd(circle, -angle))
        case MarkType.LINE =>
          drawLine(g2d, getLineEnd(circle, 0D), getLineEnd(wc, 0D))
          Set(getLineEnd(circle, 0D))
        case MarkType.DOUBLE_LINE =>
          drawLine(g2d, getLineEnd(circle, da), getLineEnd(wc, da))
          drawLine(g2d, getLineEnd(circle, -da), getLineEnd(wc, -da))
          Set(getLineEnd(circle, da), getLineEnd(circle, -da))
      }
    } else { Set.empty }
    connectors
  }

  private def drawVowel(g2d: Graphics2D, vow: Vowel, sylCircle: Circle, isDouble: Boolean): Set[Coord] = {
    val wc = getWordCircle
    val offset = (sylCircle.radius * vow.position.offset).intValue
    val center = Coord(sylCircle.center.x, sylCircle.center.y + offset)
    val rat = if (isDouble) { vow.position.doubleRatio } else { vow.position.ratio }
    val radius = (sylCircle.radius * rat).intValue
    val connectors: Set[Coord] = if (!isDouble) {
      if (vow.position.equals(VowelPosition.CENTER_IN)) {
        val from = Coord(center.x, center.y - radius)
        val to = Coord(wc.center.x, wc.center.y - wc.radius)
        drawLine(g2d, from, to)
        highlightLineConnector(g2d, from)
        Set(from)
      } else if (vow.position.equals(VowelPosition.CENTER_OUT)) {
        val from = Coord(center.x, center.y + radius)
        val to = Coord(center.x, center.y + wc.radius)
        drawLine(g2d, from, to)
        highlightLineConnector(g2d, from)
        Set.empty
      } else { Set.empty }
    } else { Set.empty }
    drawCircle(g2d, Circle(center, radius), false)
    connectors
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

  private def getRotatedDot(coord: Coord, center: Coord, angle: Double): Coord = {
    val xDiff = (coord.x - center.x)
    val yDiff = (coord.y - center.y)
    val cosA = Math.cos(Math.toRadians(angle))
    val sinA = Math.sin(Math.toRadians(angle))
    val x = center.x + (cosA * xDiff) - (sinA * yDiff)
    val y = center.y + (sinA * xDiff) + (cosA * yDiff)
    Coord(x.intValue, y.intValue)
  }

  private def getLineEnd(circle: Circle, angle: Double): Coord = {
    val x = circle.center.x - (Math.sin(angle) * circle.radius).intValue
    val y = circle.center.y - (Math.cos(angle) * circle.radius).intValue - HALF_LINE
    Coord(x, y)
  }

  private def drawPunctation(g2d: Graphics2D, pun: Punctation): Set[Coord] = {
    //val wc = getWordCircle
    //TODO implement
    Set.empty
  }

  private def drawConsonantPoint(g2d: Graphics2D, pos: Coord, size: Int): Unit = {
    g2d.fill(new Ellipse2D.Double(pos.x - size, pos.y - size, size * 2, size * 2))
  }

  private def drawArc(g2d: Graphics2D, circle: Circle, startAngle: Int, endAngle: Int, ct: CircleType): Unit = {
    val start = startAngle + 180
    val diff = Math.abs(startAngle - endAngle)
    val extent = if (ct.equals(CircleType.OPEN)) { 360 - diff } else { diff }
    val bounds = new Rectangle2D.Double(circle.xStart, circle.yStart, circle.diameter, circle.diameter)
    val arc = new Arc2D.Double(bounds, start, extent, Arc2D.OPEN)
    g2d.setStroke(STROKE)
    g2d.draw(arc)
  }

  private def drawCircle(g2d: Graphics2D, circle: Circle, fill: Boolean): Unit = {
    val ellipse = new Ellipse2D.Double(circle.xStart, circle.yStart, circle.diameter, circle.diameter)
    if (fill) {
      val oldColor = g2d.getPaint
      g2d.setPaint(g2d.getBackground)
      g2d.fill(ellipse)
      g2d.setPaint(oldColor)
    } else {
      g2d.draw(ellipse)
    }
  }

  private def drawLine(g2d: Graphics2D, from: Coord, to: Coord): Unit = {
    g2d.drawLine(from.x, from.y, to.x, to.y)
  }

  private def highlightLineConnector(g2d: Graphics2D, pos: Coord): Unit = {
    if (conceptMode) { drawPoint(g2d, pos, Color.RED) }
  }

  private def drawPoint(g2d: Graphics2D, pos: Coord, color: Color): Unit = {
    val oldColor = g2d.getPaint
    g2d.setPaint(color)
    g2d.fill(new Ellipse2D.Double(pos.x - 2D, pos.y - 2D, 4D, 4D))
    g2d.setPaint(oldColor)
  }
}

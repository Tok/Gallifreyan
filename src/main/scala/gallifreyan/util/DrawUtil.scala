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
  val conceptMode = false
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
    drawCircle(g2d, getWordCircle, false)
    if (!sent.isEmpty) {
      val rot = 360D / sent.size
      val sizeRatio = CalcUtil.getSizeRatio(sent.size)
      sent.foreach(drawSyllable(g2d, _, rot, sizeRatio))
      g2d.rotate(Math.toRadians(rot * sent.size))
    }
    if (addText) {
      g2d.drawString(sent.map(_.mkString).mkString, 10, FONT.getSize)
    }
    g2d.dispose
  }

  private def drawSyllable(g2d: Graphics2D, syl: Syllable, rot: Double, sizeRatio: Double): Unit = {
    def isDouble(i: Int, syl: Syllable): Boolean = i > 0 && syl(i) == syl(i - 1)
    syl.indices.foreach(i => drawCharacter(g2d, syl(i), getSyllableCircle(syl, sizeRatio), isDouble(i, syl), sizeRatio))
    g2d.rotate(Math.toRadians(-rot), getWordCircle.center.x, getWordCircle.center.y)
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

  private def drawCharacter(g2d: Graphics2D, c: Character, sylCircle: Circle, isDouble: Boolean, sizeRatio: Double): Unit = {
    c match {
      case con: Consonant => drawConsonant(g2d, con, isDouble, sizeRatio)
      case vow: Vowel => drawVowel(g2d, vow, sylCircle, isDouble)
      case pun: Punctation => drawPunctation(g2d, pun)
    }
    drawHelpPoint(g2d, getWordCircle.center)
  }

  private def drawConsonant(g2d: Graphics2D, con: Consonant, isDouble: Boolean, sizeRatio: Double): Unit = {
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
    if (!isDouble) {
      val da = Math.toRadians(10D)
      con.markType match {
        case MarkType.NONE =>
        case MarkType.DOUBLE_DOT =>
          val (offset, size) = getOffsetAndSize(circle, con)
          drawConsonantPoint(g2d, getDot(circle, offset + da), size)
          drawConsonantPoint(g2d, getDot(circle, offset - da), size)
        case MarkType.TRIPPLE_DOT =>
          val (offset, size) = getOffsetAndSize(circle, con)
          drawConsonantPoint(g2d, getDot(circle, offset + (da * 2D)), size)
          drawConsonantPoint(g2d, getDot(circle, offset), size)
          drawConsonantPoint(g2d, getDot(circle, offset - (da * 2D)), size)
        case MarkType.TRIPPLE_LINE =>
          val angle = da * 2D
          drawLine(g2d, getLineEnd(circle, angle), getLineEnd(wc, angle))
          drawLine(g2d, getLineEnd(circle, 0D), getLineEnd(wc, 0D))
          drawLine(g2d, getLineEnd(circle, -angle), getLineEnd(wc, -angle))
        case MarkType.LINE =>
          drawLine(g2d, getLineEnd(circle, 0D), getLineEnd(wc, 0D))
        case MarkType.DOUBLE_LINE =>
          drawLine(g2d, getLineEnd(circle, da), getLineEnd(wc, da))
          drawLine(g2d, getLineEnd(circle, -da), getLineEnd(wc, -da))
      }
    }
    drawHelpPoint(g2d, c1)
    drawHelpPoint(g2d, c2)
    drawHelpPoint(g2d, circle.center)
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

  private def getLineEnd(circle: Circle, angle: Double): Coord = {
    val x = circle.center.x - (Math.sin(angle) * circle.radius).intValue
    val y = circle.center.y - (Math.cos(angle) * circle.radius).intValue - HALF_LINE
    Coord(x, y)
  }

  private def drawVowel(g2d: Graphics2D, vow: Vowel, sylCircle: Circle, isDouble: Boolean): Unit = {
    val wc = getWordCircle
    val offset = (sylCircle.radius * vow.position.offset).intValue
    val center = Coord(sylCircle.center.x, sylCircle.center.y + offset)
    val rat = if (isDouble) { vow.position.doubleRatio } else { vow.position.ratio }
    val radius = (sylCircle.radius * rat).intValue
    if (!isDouble) {
      if (vow.position.equals(VowelPosition.CENTER_IN)) {
        val from = Coord(center.x, center.y - radius)
        val to = Coord(wc.center.x, wc.center.y - wc.radius)
        drawLine(g2d, from, to)
      }
      if (vow.position.equals(VowelPosition.CENTER_OUT)) {
        val from = Coord(center.x, center.y + radius)
        val to = Coord(center.x, center.y + wc.radius)
        drawLine(g2d, from, to)
      }
    }
    drawCircle(g2d, Circle(center, radius), false)
    drawHelpPoint(g2d, center)
  }

  private def drawPunctation(g2d: Graphics2D, pun: Punctation): Unit = {
    //val wc = getWordCircle
    //TODO implement
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

  private def drawHelpPoint(g2d: Graphics2D, pos: Coord): Unit = {
    if (conceptMode) { drawPoint(g2d, pos, Color.WHITE) }
  }

  private def drawConnectorPoint(g2d: Graphics2D, pos: Coord): Unit = {
    if (conceptMode) { drawPoint(g2d, pos, Color.RED) }
  }

  private def drawPoint(g2d: Graphics2D, pos: Coord, color: Color): Unit = {
    val oldColor = g2d.getPaint
    g2d.setPaint(color)
    g2d.fill(new Ellipse2D.Double(pos.x - 1, pos.y - 1, 2, 2))
    g2d.setPaint(oldColor)
  }
}

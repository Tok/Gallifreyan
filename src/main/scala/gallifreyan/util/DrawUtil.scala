package gallifreyan.util

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Font
import java.awt.Polygon
import java.awt.Shape
import java.awt.geom.Arc2D
import java.awt.geom.Ellipse2D
import scala.collection.immutable.TreeSet
import org.apache.batik.svggen.SVGGraphics2D
import gallifreyan.Size
import gallifreyan.engine.cases.Arc
import gallifreyan.engine.cases.Circle
import gallifreyan.engine.cases.Coord
import gallifreyan.engine.cases.Line
import gallifreyan.engine.data.Sentence
import gallifreyan.engine.data.Word
import gallifreyan.shape.ConsonantShape
import gallifreyan.shape.PunctationShape
import gallifreyan.shape.SentenceShape
import gallifreyan.shape.SyllableShape
import gallifreyan.shape.VowelShape
import gallifreyan.shape.WordShape
import gallifreyan.shape.parts.ArcCircle
import gallifreyan.shape.parts.Arcs
import gallifreyan.shape.parts.Circles
import gallifreyan.shape.parts.Dots
import gallifreyan.shape.parts.Lines

object DrawUtil {
  val STROKE = new BasicStroke(Size.lineWidth)
  val FONT = new Font(Font.MONOSPACED, Font.BOLD, 30)

  def drawSentence(g2d: SVGGraphics2D, sentence: Sentence, fg: Color, bg: Color, addText: Boolean, stubs: Boolean): Unit = {
    g2d.setPaint(fg)
    g2d.setBackground(bg)
    g2d.setStroke(STROKE)
    g2d.clearRect(0, 0, Size.width, Size.height)
    if (!sentence.v.isEmpty) {
      val sentenceShape = GenerationUtil.generateSentence(sentence)
      drawSentenceShape(g2d, sentenceShape)
      val separatedLines = GenerationUtil.separateLines(sentenceShape)
      if (stubs) {
        markStubs(g2d, separatedLines)
      } else {
        connectLinesToCircle(g2d, separatedLines, sentence)
        //TODO implement
        //val rest = connectLines(g2d, separatedLines)
        //connectLinesToCircle(g2d, rest, sentence)
      }
      if (addText) { writeText(g2d, sentence) }
    }
    g2d.dispose
  }

  private def drawSentenceShape(g2d: SVGGraphics2D, sentenceShape: SentenceShape): Unit = {
    sentenceShape.divots.foreach(_.foreach(drawArc(g2d, _)))
    sentenceShape.words.foreach(drawWordShape(g2d, _))
    if (sentenceShape.inner.isDefined) { drawArcCircle(g2d, sentenceShape.inner.get) }
    drawCircle(g2d, sentenceShape.outer)
  }

  private def drawWordShape(g2d: SVGGraphics2D, word: WordShape): Unit = {
    def drawPunctationShape(ps: PunctationShape): Unit = {
      ps.circles.foreach(drawCircles(g2d, _))
      ps.dots.foreach(drawDots(g2d, _))
      ps.lines.foreach(drawLines(g2d, _))
    }
    word.punctation.foreach(drawPunctationShape(_))
    word.syllables.foreach(drawSyllableShape(g2d, _))
    if (word.circle.isDefined) { drawArcCircle(g2d, word.circle.get) }
  }

  private def drawArcCircle(g2d: SVGGraphics2D, ac: ArcCircle): Unit = {
    if (ac.starts.isEmpty || ac.ends.isEmpty) {
      drawCircle(g2d, ac.circle)
    } else {
      val arcs = ac.starts.zip(ac.ends).map(p => Arc(ac.circle, p._1, p._2))
      arcs.foreach(drawArc(g2d, _))
    }
  }

  private def drawSyllableShape(g2d: SVGGraphics2D, syl: SyllableShape): Unit = {
    def drawConsonantShape(cs: ConsonantShape): Unit = {
      cs.round match {
        case a: Arcs => drawArcs(g2d, a)
        case c: Circles => drawCircles(g2d, c)
      }
      cs.dots.foreach(drawDots(g2d, _))
    }
    def drawVowelShape(vs: VowelShape): Unit = {
      drawCircles(g2d, vs.circles)
    }
    syl.consonant.foreach(drawConsonantShape(_))
    syl.vowel.foreach(drawVowelShape(_))
  }

  private def drawArcs(g2d: SVGGraphics2D, arcs: Arcs): Unit = {
    drawArc(g2d, arcs.inner)
    arcs.outer.foreach(drawArc(g2d, _))
  }

  private def drawCircles(g2d: SVGGraphics2D, c: Circles): Unit = {
    drawCircle(g2d, c.inner)
    c.outer.foreach(drawCircle(g2d, _))
  }

  private def drawDots(g2d: SVGGraphics2D, d: Dots): Unit = {
    d.left.foreach(fillCircle(g2d, _))
    d.middle.foreach(fillCircle(g2d, _))
    d.right.foreach(fillCircle(g2d, _))
  }

  private def drawLines(g2d: SVGGraphics2D, l: Lines): Unit = {
    l.left.foreach(drawLine(g2d, _))
    l.middle.foreach(drawLine(g2d, _))
    l.right.foreach(drawLine(g2d, _))
  }

  private def markStubs(g2d: SVGGraphics2D, connectorLines: List[Line]): Unit = connectorLines.foreach(drawLine(g2d, _))

  private def connectLinesToCircle(g2d: SVGGraphics2D, connectorLines: List[Line], sentence: Sentence): Unit = {
    def circle: Circle = if (sentence.isSingleWord) { Word.outer(Size.lineWidth) } else { Sentence.outer(Size.lineWidth) }
    def calcInter(line: Line): Coord = CalcUtil.calcIntersection(line.start, line.end, circle)
    def draw(start: Coord, end: Coord): Unit = {
      if (!sentence.isSingleWord || CalcUtil.calcDistance(start, circle.calcClosestTo(start)) < CalcUtil.calcDistance(end, circle.calcClosestTo(start))) {
        drawLine(g2d, start, end)
      } else {
        drawLine(g2d, start, circle.moveFromCenter(start, 1.2D))
      }
    }
    connectorLines.foreach(line => draw(line.start, calcInter(line)))
  }

  private def connectLines(g2d: SVGGraphics2D, connectorLines: List[Line]): List[Line] = {
    //FIXME prevent mutli connections
    //FIXME always connect closest pairs
    //FIXME prevent striking through unconnected consonants and vowels
    val orb = 20
    val cent = Sentence.circle.center
    def pointToEachother(first: Int, second: Int): Boolean = {
      val diff = Math.abs(Math.abs(first) - Math.abs(second))
      diff >= 180 - orb && diff <= 180 + orb
    }
    def endsCloserThanStarts(first: Line, second: Line): Boolean = {
      val startDist = CalcUtil.calcDistance(first.start, second.start)
      val endDist = CalcUtil.calcDistance(first.end, second.end)
      endDist <= startDist
    }
    def orderLine(line: Line): Line = {
      if (line.start.x + line.start.y <= line.end.x + line.end.y) { line } else { Line(line.end, line.start) }
    }
    val connectorAngles: List[(Line, Int)] = connectorLines.map(l => (l, (CalcUtil.calcAngle(l.start, l.end) + 360) % 360))
    val lineOptions: List[Option[Line]] = for {
      tup1: (Line, Int) <- connectorAngles
      tup2: (Line, Int) <- connectorAngles
    } yield {
      if (pointToEachother(tup1._2, tup2._2) && endsCloserThanStarts(tup1._1, tup2._1)) {
        Some(Line(tup1._1.start, tup2._1.start))
      } else {
        None
      }
    }
    val filtered = lineOptions.filterNot(_ == None).map(_.get)
    println("filtered: " + filtered)
    val distanced = filtered.map(l => (l, CalcUtil.calcDistance(l.start, l.end))).sortBy(_._2).map(_._1)
    println("distanced: " + distanced)
    val lines = distanced.map(orderLine(_)).distinct
    println("lines: " + lines)
    val uniq = lines.filterNot(l => lines.map(_.end).contains(l.start))
    println("uniq: " + uniq)
    val noMulti = uniq.map(_.start).distinct.map(s => uniq.find(s == _.start)).filterNot(_ == None).map(_.get)   
    println("noMulti: " + noMulti)
    noMulti.foreach(l => drawLine(g2d, l))
    connectorLines.filterNot(l => noMulti.map(_.start).contains(l.start) || uniq.map(_.end).contains(l.start))
  }

  private def writeText(g2d: SVGGraphics2D, sentence: Sentence): Unit = g2d.drawString(sentence.mkString, 10F, FONT.getSize.floatValue)

  private def drawConsonantPoint(g2d: SVGGraphics2D, pos: Coord, size: Int): Unit = {
    val doubleSize = size * 2
    g2d.fill(new Ellipse2D.Double(pos.x - size, pos.y - size, doubleSize, doubleSize))
  }

  private def fillRect(g2d: SVGGraphics2D, center: Coord, p1: Coord, p2: Coord): Unit = {
    val p3 = CalcUtil.rotate(p1, 180D, center)
    val p4 = CalcUtil.rotate(p2, 180D, center)
    val x: Array[Int] = Array(p1.x, p2.x, p3.x, p4.x)
    val y: Array[Int] = Array(p1.y, p2.y, p3.y, p4.y)
    val triangle = new Polygon(x, y, 4)
    drawOrFill(g2d, triangle, true)
  }

  private def drawArc(g2d: SVGGraphics2D, arc: Arc): Unit = drawArc(g2d, arc.circle, arc.start, arc.end)
  private def drawArc(g2d: SVGGraphics2D, circle: Circle, s: Coord, e: Coord): Unit = {
    val arc = new Arc2D.Double(Arc2D.OPEN)
    arc.setFrame(circle.xStart, circle.yStart, circle.diameter, circle.diameter)
    arc.setAngles(s.x, s.y, e.x, e.y)
    g2d.setStroke(STROKE)
    drawOrFill(g2d, arc, false)
  }

  private def fillCircle(g2d: SVGGraphics2D, circle: Circle): Unit = drawOrFillCircle(g2d, circle, true)
  private def drawCircle(g2d: SVGGraphics2D, circle: Circle): Unit = drawOrFillCircle(g2d, circle, false)
  private def drawOrFillCircle(g2d: SVGGraphics2D, circle: Circle, fill: Boolean): Unit = {
    val ellipse = new Ellipse2D.Double(circle.xStart, circle.yStart, circle.diameter, circle.diameter)
    drawOrFill(g2d, ellipse, fill)
  }

  private def drawOrFill(g2d: SVGGraphics2D, shape: Shape, fill: Boolean): Unit = {
    if (!fill) { g2d.draw(shape) } else {
      val oldColor = g2d.getPaint
      g2d.setPaint(g2d.getBackground)
      g2d.fill(shape)
      g2d.setPaint(oldColor)
    }
  }

  private def drawLine(g2d: SVGGraphics2D, l: Line): Unit = drawLine(g2d, l.start, l.end)
  private def drawLine(g2d: SVGGraphics2D, s: Coord, e: Coord): Unit = {
    //This method is using polyline instead of line to allow better manipulation of the resulting SVG
    //g2d.drawLine(s.x, s.y, e.x, e.y)
    g2d.drawPolyline(List(s.x, e.x).toArray, List(s.y, e.y).toArray, 2)
  }
}

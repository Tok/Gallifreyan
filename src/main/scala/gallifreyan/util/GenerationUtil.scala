package gallifreyan.util

import gallifreyan.engine.cases.Circle
import gallifreyan.engine.characters.Vowel
import gallifreyan.engine.characters.Consonant
import gallifreyan.engine.CircleType
import gallifreyan.engine.VowelPosition
import gallifreyan.engine.cases.Line
import gallifreyan.shape.VowelShape
import gallifreyan.shape.parts.Circles
import gallifreyan.shape.ConsonantShape
import gallifreyan.engine.MarkType
import gallifreyan.shape.parts.Round
import gallifreyan.shape.parts.Arcs
import gallifreyan.engine.cases.Arc
import gallifreyan.shape.parts.Lines
import gallifreyan.shape.parts.Dots
import gallifreyan.engine.cases.Coord
import gallifreyan.engine.characters.Punctation
import gallifreyan.engine.data.Sentence
import gallifreyan.shape.PunctationShape
import gallifreyan.engine.data.Syllable
import gallifreyan.shape.parts.Arcs
import gallifreyan.shape.SyllableShape
import gallifreyan.shape.WordShape
import gallifreyan.engine.data.Word
import gallifreyan.shape.SentenceShape

object GenerationUtil {
  val LINE_WIDTH = 2
  val HALF_LINE = LINE_WIDTH / 2
  val CONN_MARK_SIZE = 20

  def generateSentence(sentence: Sentence): SentenceShape = {
    val originalDivotCircle = Sentence.circle.addToRadius((Sentence.circle.radius * 0.5D).intValue)
    val center = Coord(originalDivotCircle.center.x, originalDivotCircle.center.y + originalDivotCircle.radius)
    val radius = Sentence.circle.radius * CalcUtil.calcSizeRatio(sentence.v.size + 1)
    val circle = Circle(center, radius.intValue)
    val (s, e) = CalcUtil.calcStartAndEnd(Sentence.circle, circle)
    def generateDivot(angle: Double): Arc = {
      val rotE = CalcUtil.rotate(e, angle, Sentence.circle.center)
      val rotS = CalcUtil.rotate(s, angle, Sentence.circle.center)
      val rotatedCircle = Circle(CalcUtil.rotate(center, angle, Sentence.circle.center), radius.intValue)
      Arc(rotatedCircle, rotE, rotS)
    }
    val divots: Map[Double, Arc] = {
      val angle = sentence.rots(1) / 2
      val angles = sentence.rots.map(r => (r * -1) - angle)
      angles.map(a => (a, generateDivot(a))).toMap
    }
    val sentSizeRatio = CalcUtil.calcSizeRatio(sentence.v.size)
    def getRotated(angle: Double): Circle = divots.get(angle).get.circle
    val outer = Sentence.outer(LINE_WIDTH)
    val words = sentence.zipRots.map(z => generateWord(z._1, z._2, sentSizeRatio, getRotated(z._2), outer))
    SentenceShape(Sentence.circle, Sentence.outer(LINE_WIDTH), words, Some(divots.values.toList))
  }

  def generateSentenceFromSingleWord(word: Word): SentenceShape = {
    val wordSizeRatio = CalcUtil.calcSizeRatio(word.v.size)
    val syllables = word.zipRots.map(z => generateSyllable(z._1, z._2, wordSizeRatio, Word.circle))
    val wordShape = WordShape(None, syllables, None)    
    SentenceShape(Word.circle, Word.outer(LINE_WIDTH), List(wordShape), None)
  }

  private def generateWord(word: Word, sentRot: Double, sentRatio: Double, divCirc: Circle, outer: Circle): WordShape = {
    def wc: Circle = {
      val sc = Sentence.circle
      val offset = (sc.radius * 0.6D).intValue
      val radius = (sc.radius * 0.35D * sentRatio).intValue
      val center = CalcUtil.rotate(sc.center.addToY(offset), -sentRot, sc.center)
      Circle(center, radius)
    }
    val wordSizeRatio = CalcUtil.calcSizeRatio(word.v.size)
    val syllables = word.zipRots.map(z => generateSyllable(z._1, z._2, wordSizeRatio, wc))
    val punctation: Option[PunctationShape] = {
      val char = word.v.last.v.last
      if (!char.isInstanceOf[Punctation]) { None } else {
        val pun = char.asInstanceOf[Punctation]
        Some(generatePunctation(pun, divCirc, outer))
      }
    }
    WordShape(Some(wc), syllables, punctation)
  }

  private def generateSyllable(syl: Syllable, rot: Double, sizeRatio: Double, wc: Circle): SyllableShape = {
    val consonantShape: Option[ConsonantShape] = syl.v.head match {
      case con: Consonant =>
        val isDouble = !syl.v.tail.isEmpty && syl.v.tail.head == con
        Some(generateConsonantShape(con, isDouble, sizeRatio, -rot, wc))
      case _ => None
    }
    val vowels = syl.v.filter(_.isInstanceOf[Vowel])
    val vowelShape: Option[VowelShape] = {
      if (vowels.isEmpty) { None } else {
        vowels.head match {
          case vow: Vowel =>
            val sylCircle = makeSylCircle(syl, sizeRatio, wc)
            val lastCon = syl.v.head match { case con: Consonant => Some(con); case _ => None }
            val isDouble = !vowels.tail.isEmpty && vowels.tail.head == vow
            Some(generateVowelShape(vow, sylCircle, lastCon, isDouble, -rot, wc))
          case _ => None
        }
      }
    }
    SyllableShape(consonantShape, vowelShape)
  }

  private def generateConsonantShape(con: Consonant, isDouble: Boolean, sizeRatio: Double, rot: Double, wc: Circle): ConsonantShape = {
    val original = makeConCircle(con, wc, false, sizeRatio)
    val originalOuter = if (isDouble) { Some(makeConCircle(con, wc, isDouble, sizeRatio)) } else { None }
    val circle = Circle(CalcUtil.rotate(original.center, rot, wc.center), original.radius)
    val outer = if (isDouble) { Some(Circle(CalcUtil.rotate(originalOuter.get.center, rot, wc.center), originalOuter.get.radius)) } else { None }
    def connCircle: Circle = original.addToRadius(CONN_MARK_SIZE)
    val round: Round = if (!con.circleType.isCrossing) {
      Circles(circle, outer)
    } else {
      val (s, e) = CalcUtil.calcStartAndEnd(wc, original)
      val rotE = CalcUtil.rotate(e, rot, wc.center)
      val rotS = CalcUtil.rotate(s, rot, wc.center)
      val inner = Arc(circle, rotE, rotS)
      if (isDouble) {
        val (sD, eD) = CalcUtil.calcStartAndEnd(wc, outer.get)
        val rotEd = CalcUtil.rotate(eD, rot, wc.center)
        val rotSd = CalcUtil.rotate(sD, rot, wc.center)
        Arcs(inner, Some(Arc(outer.get, rotEd, rotSd)))
      } else {
        //con.circleType match {
        //  case CircleType.HALF => fillRect(g2d, circle.center, rotE, rotS)
        //  case _ => fillCircle(g2d, circle.addToRadius(-HALF_LINE))
        //}
        Arcs(inner, None)
      }
    }
    def da: Double = Math.toRadians(10D)
    def dda: Double = da * 2D
    val lines: Option[Lines] = con.markType match {
      case MarkType.TRIPPLE_LINE =>
        val leftStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, dda, HALF_LINE), rot, wc.center)
        val middleStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, 0D, HALF_LINE), rot, wc.center)
        val rightStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, -dda, HALF_LINE), rot, wc.center)
        val leftEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, dda, HALF_LINE), rot, wc.center)
        val middleEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, 0D, HALF_LINE), rot, wc.center)
        val rightEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, -dda, HALF_LINE), rot, wc.center)
        Some(Lines(Some(Line(leftStart, leftEnd)), Some(Line(middleStart, middleEnd)), Some(Line(rightStart, rightEnd))))
      case MarkType.LINE =>
        val start = CalcUtil.rotate(CalcUtil.calcLineEnd(original, 0D, HALF_LINE), rot, wc.center)
        val end = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, 0D, HALF_LINE), rot, wc.center)
        Some(Lines(None, Some(Line(start, end)), None))
      case MarkType.DOUBLE_LINE =>
        val leftStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, da, HALF_LINE), rot, wc.center)
        val rightStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, -da, HALF_LINE), rot, wc.center)
        val leftEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, da, HALF_LINE), rot, wc.center)
        val rightEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, -da, HALF_LINE), rot, wc.center)
        Some(Lines(Some(Line(leftStart, leftEnd)), None, Some(Line(rightStart, rightEnd))))
      case _ => None
    }
    def daDot: Double = Math.toRadians(8D)
    def ddaDot: Double = daDot * 2D
    val dots: Option[Dots] = con.markType match {
      case MarkType.DOUBLE_DOT =>
        val (offset, size) = CalcUtil.calcOffsetAndSize(circle, con)
        val left = Circle(CalcUtil.rotate(CalcUtil.calcDot(original, offset + daDot), rot, wc.center), size)
        val right = Circle(CalcUtil.rotate(CalcUtil.calcDot(original, offset - daDot), rot, wc.center), size)
        Some(Dots(Some(left), None, Some(right)))
      case MarkType.TRIPPLE_DOT =>
        val (offset, size) = CalcUtil.calcOffsetAndSize(circle, con)
        val left = Circle(CalcUtil.rotate(CalcUtil.calcDot(original, offset + ddaDot), rot, wc.center), size)
        val middle = Circle(CalcUtil.rotate(CalcUtil.calcDot(original, offset), rot, wc.center), size)
        val right = Circle(CalcUtil.rotate(CalcUtil.calcDot(original, offset - ddaDot), rot, wc.center), size)
        Some(Dots(Some(left), Some(middle), Some(right)))
      case _ => None
    }
    ConsonantShape(round, lines, dots)
  }

  private def generateVowelShape(vow: Vowel, sylCircle: Circle, lastCon: Option[Consonant], isDouble: Boolean, rot: Double, wc: Circle): VowelShape = {
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
    def makeRadius(ratio: Double): Int = {
      lastCon match {
        case Some(c) if c.circleType.equals(CircleType.HALF) => (sylCircle.radius * ratio * CircleType.openHalfRatio).intValue
        case _ => (sylCircle.radius * ratio).intValue
      }
    }
    val center = CalcUtil.rotate(original, rot, wc.center)
    val radius = makeRadius(vow.position.ratio)
    val circle: Circle = Circle(center, radius)
    val outer: Option[Circle] = if (isDouble) { Some(Circle(center, makeRadius(vow.position.doubleRatio))) } else { None }
    val line: Option[Line] = {
      if (vow.position.equals(VowelPosition.CENTER_IN)) {
        val from = CalcUtil.rotate(original.addToY(-radius), rot, wc.center)
        val to = CalcUtil.rotate(original.addToY(-radius - CONN_MARK_SIZE), rot, wc.center)
        Some(Line(from, to))
      } else if (vow.position.equals(VowelPosition.CENTER_OUT)) {
        val from = CalcUtil.rotate(original.addToY(radius), rot, wc.center)
        val to = CalcUtil.rotate(original.addToY(radius + CONN_MARK_SIZE), rot, wc.center)
        Some(Line(from, to))
      } else { None }
    }
    VowelShape(Circles(circle, outer), line)
  }

  private def generatePunctation(pun: Punctation, cir: Circle, outer: Circle): PunctationShape = {
    def dotSize = 5
    def circleSize = 20
    def dualAngle = 10
    def trippleAngle = 15
    def ratio = 0.9D
    def close: Coord = cir.calcClosestTo(Sentence.circle.center)
    pun match {
      case Punctation.DOT =>
        val circle = Circle(close, circleSize)
        PunctationShape(Some(Circles(circle, None)), None, None)
      case Punctation.QUESTION =>
        val left = Circle(CalcUtil.rotate(cir.moveFromCenter(close, ratio), -dualAngle, cir.center), dotSize)
        val right = Circle(CalcUtil.rotate(cir.moveFromCenter(close, ratio), dualAngle, cir.center), dotSize)
        PunctationShape(None, None, Some(Dots(Some(left), None, Some(right))))
      case Punctation.EXCLAIM =>
        val left = Circle(CalcUtil.rotate(cir.moveFromCenter(close, ratio), -trippleAngle, cir.center), dotSize)
        val middle = Circle(cir.moveFromCenter(close, ratio), dotSize)
        val right = Circle(CalcUtil.rotate(cir.moveFromCenter(close, ratio), trippleAngle, cir.center), dotSize)
        PunctationShape(None, None, Some(Dots(Some(left), Some(middle), Some(right))))
      case Punctation.DOUBLEQUOTE =>
        val middle = Line(close, outer.calcClosestTo(close))
        PunctationShape(None, Some(Lines(None, Some(middle), None)), None)
      case Punctation.QUOTE =>
        val first = CalcUtil.rotate(close, -dualAngle, cir.center)
        val second = CalcUtil.rotate(close, dualAngle, cir.center)
        val left = Line(first, CalcUtil.rotate(outer.calcClosestTo(close), dualAngle, outer.center))
        val right = Line(second, CalcUtil.rotate(outer.calcClosestTo(close), -dualAngle, outer.center))
        PunctationShape(None, Some(Lines(Some(left), None, Some(right))), None)
      case Punctation.HYPHEN =>
        val first = CalcUtil.rotate(close, -trippleAngle, cir.center)
        val second = CalcUtil.rotate(close, trippleAngle, cir.center)
        val left = Line(first, CalcUtil.rotate(outer.calcClosestTo(close), dualAngle, outer.center))
        val middle = Line(close, outer.calcClosestTo(close))
        val right = Line(second, CalcUtil.rotate(outer.calcClosestTo(close), -dualAngle, outer.center))
        PunctationShape(None, Some(Lines(Some(left), Some(middle), Some(right))), None)
      case Punctation.COMMA =>
        val dot = Circle(close, circleSize)
        PunctationShape(None, None, Some(Dots(None, Some(dot), None)))
      case Punctation.SEMICOLON =>
        val dot = Circle(cir.moveFromCenter(close, ratio), dotSize)
        PunctationShape(None, None, Some(Dots(None, Some(dot), None)))
      case Punctation.COLON =>
        val outer = Circle(close, circleSize)
        val inner = Circle(close, (circleSize * 4) / 5)
        PunctationShape(Some(Circles(inner, Some(outer))), None, None)
    }
  }

  def makeConCircle(con: Consonant, wordCircle: Circle, isDouble: Boolean, sizeRatio: Double): Circle = {
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

  def makeSylCircle(syl: Syllable, sizeRatio: Double, wordCircle: Circle): Circle = {
    syl.v(0) match {
      case con: Consonant => GenerationUtil.makeConCircle(con, wordCircle, false, sizeRatio)
      case _ => GenerationUtil.makeConCircle(Consonant.TH, wordCircle, false, sizeRatio)
      //Vowels in syllables without constants use the same circle as Th
    }
  }
}

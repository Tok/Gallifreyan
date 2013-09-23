package gallifreyan.util

import scala.annotation.tailrec

import gallifreyan.Size
import gallifreyan.engine.CircleType
import gallifreyan.engine.MarkType
import gallifreyan.engine.VowelPosition
import gallifreyan.engine.cases.Arc
import gallifreyan.engine.cases.Circle
import gallifreyan.engine.cases.Coord
import gallifreyan.engine.cases.Line
import gallifreyan.engine.characters.Consonant
import gallifreyan.engine.characters.Punctation
import gallifreyan.engine.characters.Vowel
import gallifreyan.engine.data.Sentence
import gallifreyan.engine.data.Syllable
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
import gallifreyan.shape.parts.Round

object GenerationUtil {
  val CONN_MARK_SIZE = 20

  def generateSentence(sentence: Sentence): SentenceShape = {
    if (!sentence.isSingleWord) { generateSentenceFromWords(sentence) }
    else { generateSentenceFromSingleWord(sentence.v.head) }
  }

  private def generateSentenceFromWords(sentence: Sentence): SentenceShape = {
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
      val angles = sentence.rots.map(r => (r, (r * -1) - angle))
      angles.map(tup => (tup._1, generateDivot(tup._2))).toMap
    }
    def getRotated(angle: Double): Circle = divots.get(angle).get.circle
    val outer = Sentence.outer(Size.lineWidth)
    val sentSizeRatio = CalcUtil.calcSizeRatio(sentence.v.size)
    val offset = (Sentence.circle.radius * 0.6D).intValue
    val rad = (Sentence.circle.radius * 0.35D * sentSizeRatio).intValue
    val words = sentence.zipRots.map(z => generateWord(z._1, z._2, offset, rad, getRotated(z._2), outer))
    val arcs = divots.values.toList.map(a => Arc(Sentence.circle, a.start, a.end))
    val switched = switchEndpoints(arcs, Sentence.circle, arcs.size >= 5)
    val arcCircle = ArcCircle(Sentence.circle, switched.map(_.start), switched.map(_.end))
    SentenceShape(Some(arcCircle), Sentence.outer(Size.lineWidth), words, Some(divots.values.toList))
  }

  private def generateSentenceFromSingleWord(word: Word): SentenceShape = {
    if (word.v.isEmpty) {
      SentenceShape(None, Word.outer(Size.lineWidth), Nil, None)
    } else {
      val sizeRatio = CalcUtil.calcSizeRatio(word.v.size)
      val wordShape = generateWord(word, 0D, 0, Word.circle.radius, Word.circle, Word.outer(Size.lineWidth))
      SentenceShape(None, Word.outer(Size.lineWidth), List(wordShape), None)
    }
  }

  private def generateWord(word: Word, sentRot: Double, offset: Int, radius: Int, divCirc: Circle, outer: Circle): WordShape = {
    def wc: Circle = {
      val center = CalcUtil.rotate(Sentence.circle.center.addToY(offset), -sentRot, Sentence.circle.center)
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
    def getStartAndEnd(co: Option[ConsonantShape]): Option[(Coord, Coord)] = {
      if (co.isDefined && co.get.round.isInstanceOf[Arcs]) {
        val arcs = co.get.round.asInstanceOf[Arcs]
        Some(arcs.inner.start, arcs.inner.end)
      } else { None }
    }
    val points = syllables.map(s => getStartAndEnd(s.consonant))
    val arcs = points.filter(_.isDefined).map(_.get).map(tup => Arc(wc, tup._1, tup._2))
    val switched = switchEndpoints(arcs, wc, false)
    val arcCircle = ArcCircle(wc, switched.map(_.start), switched.map(_.end))
    WordShape(Some(arcCircle), syllables, punctation)
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
      } else { Arcs(inner, None) }
    }
    def da: Double = Math.toRadians(10D)
    def dda: Double = da * 2D
    val lines: Option[Lines] = con.markType match {
      case MarkType.TRIPPLE_LINE =>
        val leftStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, dda, Size.halfLine), rot, wc.center)
        val middleStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, 0D, Size.halfLine), rot, wc.center)
        val rightStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, -dda, Size.halfLine), rot, wc.center)
        val leftEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, dda, Size.halfLine), rot, wc.center)
        val middleEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, 0D, Size.halfLine), rot, wc.center)
        val rightEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, -dda, Size.halfLine), rot, wc.center)
        Some(Lines(Some(Line(leftStart, leftEnd)), Some(Line(middleStart, middleEnd)), Some(Line(rightStart, rightEnd))))
      case MarkType.LINE =>
        val start = CalcUtil.rotate(CalcUtil.calcLineEnd(original, 0D, Size.halfLine), rot, wc.center)
        val end = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, 0D, Size.halfLine), rot, wc.center)
        Some(Lines(None, Some(Line(start, end)), None))
      case MarkType.DOUBLE_LINE =>
        val leftStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, da, Size.halfLine), rot, wc.center)
        val rightStart = CalcUtil.rotate(CalcUtil.calcLineEnd(original, -da, Size.halfLine), rot, wc.center)
        val leftEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, da, Size.halfLine), rot, wc.center)
        val rightEnd = CalcUtil.rotate(CalcUtil.calcLineEnd(connCircle, -da, Size.halfLine), rot, wc.center)
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
    def dotSize: Int = 5
    def circleSize: Int = 20
    def dualAngle: Int = 10
    def trippleAngle: Int = 15
    def ratio: Double = 0.9D
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

  def separateLines(sentenceShape: SentenceShape): List[Line] = {
    val syls: List[SyllableShape] = sentenceShape.words.map(_.syllables).flatten
    val cons: List[ConsonantShape] = syls.map(_.consonant).filter(_.isDefined).map(_.get)
    val vows: List[VowelShape] = syls.map(_.vowel).filter(_.isDefined).map(_.get)
    val conLinesGroups: List[Lines] = cons.map(_.lines).filter(_.isDefined).map(_.get)
    val lefts: List[Line] = conLinesGroups.map(_.left).filter(_.isDefined).map(_.get)
    val middles: List[Line] = conLinesGroups.map(_.middle).filter(_.isDefined).map(_.get)
    val rights: List[Line] = conLinesGroups.map(_.right).filter(_.isDefined).map(_.get)
    val vowLines: List[Line] = vows.map(_.line).filter(_.isDefined).map(_.get)
    lefts ::: middles ::: rights ::: vowLines
  }

  private def switchEndpoints(divots: List[Arc], circle: Circle, reverse: Boolean): List[Arc] = {
    if (divots.isEmpty) { Nil } else {
      val first = if (reverse) {
        Arc(circle, divots.head.start, divots.last.end)
      } else {
        Arc(circle, divots.last.start, divots.head.end)
      }
      @tailrec
      def rest(l: List[Arc], accu: List[Arc]): List[Arc] = {
        if (l.size <= 1) { accu }
        else {
          if (reverse) {
            rest(l.tail, accu ::: List(Arc(circle, l.tail.head.start, l.head.end)))
          } else {
            rest(l.tail, accu ::: List(Arc(circle, l.head.start, l.tail.head.end)))
          }
        }
      }
      rest(divots, List(first))
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

  private def makeSylCircle(syl: Syllable, sizeRatio: Double, wordCircle: Circle): Circle = {
    syl.v(0) match {
      case con: Consonant => GenerationUtil.makeConCircle(con, wordCircle, false, sizeRatio)
      case _ => GenerationUtil.makeConCircle(Consonant.TH, wordCircle, false, sizeRatio)
      //Vowels in syllables without constants use the same circle as Th
    }
  }
}

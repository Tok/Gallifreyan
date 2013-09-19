package gallifreyan.util

import java.util.Locale

import scala.Array.canBuildFrom
import scala.annotation.tailrec

import gallifreyan.engine.characters.Consonant
import gallifreyan.engine.characters.Punctation
import gallifreyan.engine.characters.Vowel
import gallifreyan.engine.data.Sentence
import gallifreyan.engine.data.Syllable
import gallifreyan.engine.data.Word
import gallifreyan.engine.data.Character

object TextUtil {
  def makeChar(in: String): Character = {
    def other: Character = {
      in.toUpperCase(Locale.getDefault) match {
        case "C" => Consonant.K
        case "Q" => Consonant.QU
        case _ => throw new IllegalArgumentException("Untransliteratable character: " + in)
      }
    }
    def pun: Character = Punctation.valueOf(in).getOrElse(other)
    def con: Character = Consonant.valueOf(in).getOrElse(pun)
    Vowel.valueOf(in).getOrElse(con)
  }

  def makeSentence(in: String): Sentence = Sentence(in.split(" ").map(makeWord(_)).toList)

  def makeWord(in: String): Word = {
    @tailrec
    def makeWord(accu: List[Syllable], chars: List[Char]): List[Syllable] = {
      def newChar: Character = {
        val first = chars.head.toString
        val both = if (chars.size > 1) { first + chars.tail.head.toString } else { first }
        if (isDouble(both)) { makeChar(both) } else { makeChar(first) }
      }
      def addToLast: List[Syllable] = accu.init ::: List(accu.last.addChar(newChar))
      def addAsNew: List[Syllable] = accu ::: List(Syllable(List(newChar)))
      def isBeforeLastAndLastEqual: Boolean = isLastEqual && !accu.last.v.init.isEmpty && accu.last.v.init.last == newChar
      def isLastEqual: Boolean = accu.last.v.last == newChar
      def isLastVowel: Boolean = accu.last.v.last.isInstanceOf[Vowel]
      def isLastPunctation: Boolean = accu.last.v.last.isInstanceOf[Punctation]
      def getNext: List[Char] = {
        val skip = if (chars.size > 2) { chars.tail.tail } else { Nil }
        if (newChar.isDouble) { skip } else { chars.tail }
      }
      if (chars.isEmpty) { accu }
      else if (accu.isEmpty) {
        if (newChar.isInstanceOf[Punctation]) { throw new IllegalArgumentException("Initial punctation is not allowed.") }
        else { makeWord(addAsNew, getNext) }
      } else {
        newChar match {
          case c: Consonant =>
            if (!isLastEqual || isBeforeLastAndLastEqual) { makeWord(addAsNew, getNext) }
            else { makeWord(addToLast, chars.tail) }
          case v: Vowel =>
            if ((!isLastEqual && isLastVowel) || isBeforeLastAndLastEqual) { makeWord(addAsNew, chars.tail) }
            else { makeWord(addToLast, chars.tail) }
          case _ => {
            if (isLastPunctation) { throw new IllegalArgumentException("Multi punctation is not allowed.") }
            else { makeWord(addToLast, chars.tail) }
          }
        }
      }
    }
    Word(makeWord(Nil, in.toUpperCase(Locale.getDefault).toCharArray.toList))
  }

  private def isDouble(s: String): Boolean = {
    s.equals("CH") || s.equals("SH") || s.equals("TH") || s.equals("NG") || s.equals("QU")
  }
}

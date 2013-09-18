package gallifreyan.util

import scala.annotation.tailrec
import gallifreyan.engine.traits.Character
import gallifreyan.engine.Sentence
import gallifreyan.engine.characters.Consonant
import gallifreyan.engine.characters.Punctation
import gallifreyan.engine.characters.Vowel
import java.util.Locale

object TextUtil {
  def getChar(in: String): Character = {
    def other: Character = {
      in.toUpperCase(Locale.getDefault) match {
        case "C" => Consonant.K
        case "Q" => Consonant.QU
        case _ => Punctation.SPACE
      }
    }
    def pun: Character = Punctation.valueOf(in).getOrElse(other)
    def con: Character = Consonant.valueOf(in).getOrElse(pun)
    Vowel.valueOf(in).getOrElse(con)
  }

  def getSyllables(in: String): Sentence = {
    @tailrec
    def makeSentence(accu: Sentence, chars: List[Char]): Sentence = {
      def newChar: Character = {
        val first = chars.head.toString
        val both = if (chars.size > 1) { first + chars.tail.head.toString } else { first }
        if (isDouble(both)) { getChar(both) } else { getChar(first) }
      }
      def addToLast: Sentence = accu.init ::: List(accu.last ::: List(newChar))
      def addAsNew: Sentence = accu ::: List(List(newChar))
      def isLastEqual: Boolean = accu.last.last == newChar
      def isLastVowel: Boolean = accu.last.last.isInstanceOf[Vowel]
      def getNext: List[Char] = {
        val skip = if (chars.size > 2) { chars.tail.tail } else { Nil }
        if (newChar.isDouble) { skip } else { chars.tail }
      }
      if (chars.isEmpty) { accu }
      else if (accu.isEmpty) { makeSentence(accu ::: List(List(newChar)), getNext) }
      else {
        newChar match {
          case c: Consonant =>
            if (isLastEqual) { makeSentence(addToLast, chars.tail) }
            else { makeSentence(addAsNew, getNext) }
          case v: Vowel =>
            if (isLastEqual || !isLastVowel) { makeSentence(addToLast, chars.tail) }
            else { makeSentence(addAsNew, chars.tail) }
          case _ => makeSentence(addToLast, chars.tail)
        }
      }
    }
    makeSentence(Nil, in.toUpperCase(Locale.getDefault).toCharArray.toList)
  }

  private def isDouble(s: String): Boolean = {
    s.equals("CH") || s.equals("SH") || s.equals("TH") || s.equals("NG") || s.equals("QU")
  }
}

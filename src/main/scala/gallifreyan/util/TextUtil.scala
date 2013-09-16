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
    Vowel.valueOf(in) match {
      case Some(v) => v
      case _ => Punctation.valueOf(in) match {
        case Some(p) => p
        case _ => Consonant.valueOf(in) match {
          case Some(c) => c
          case _ if in.equalsIgnoreCase("C") => Consonant.K
          case _ if in.equalsIgnoreCase("Q") => Consonant.QU
          case _ => Punctation.SPACE
        }
      }
    }
  }

  def getSyllables(in: String): Sentence = {
    def isDouble(s: String): Boolean = {
      s.equals("CH") || s.equals("SH") || s.equals("TH") || s.equals("NG") || s.equals("QU")
    }
    @tailrec
    def makeSentence(accu: Sentence, chars: List[Char]): Sentence = {
      def getNext(newChar: Character): List[Char] = {
        val skip = if (chars.size > 2) { chars.tail.tail } else { Nil }
        if (newChar.isDouble) { skip } else { chars.tail }
      }
      if (chars.isEmpty) { accu }
      else {
        val first = chars.head.toString
        val both = if (chars.size > 1) { first + chars.tail.head.toString } else { first }
        val newChar = if (isDouble(both)) { getChar(both) } else { getChar(first) }
        if (accu.isEmpty) {
          makeSentence(accu ::: List(List(newChar)), getNext(newChar))
        } else if (newChar.isInstanceOf[Consonant]) {
          if (accu.last.last == newChar) {
            makeSentence(accu.init ::: List(accu.last ::: List(newChar)), chars.tail)
          } else {
            makeSentence(accu ::: List(List(newChar)), getNext(newChar))
          }
        } else {
          makeSentence(accu.init ::: List(accu.last ::: List(newChar)), chars.tail)
        }
      }
    }
    makeSentence(Nil, in.toUpperCase(Locale.getDefault).toCharArray.toList)
  }
}

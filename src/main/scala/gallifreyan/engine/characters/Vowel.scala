package gallifreyan.engine.characters

import gallifreyan.engine.VowelPosition
import gallifreyan.engine.data.Character

sealed abstract class Vowel(val position: VowelPosition) extends Character {
  val isDouble = false
}

object Vowel {
  case object A extends Vowel(VowelPosition.OUT)
  case object E extends Vowel(VowelPosition.CENTER)
  case object I extends Vowel(VowelPosition.CENTER_IN)
  case object O extends Vowel(VowelPosition.IN)
  case object U extends Vowel(VowelPosition.CENTER_OUT)
  val values: List[Vowel] = List(A, E, I, O, U)
  val charValues: List[Character] = values.map(_.asInstanceOf[Character])
  def valueOf(name: String): Option[Vowel] = values.find(_.toString.equalsIgnoreCase(name))
}

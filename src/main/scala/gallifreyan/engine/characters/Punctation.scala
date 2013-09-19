package gallifreyan.engine.characters

import gallifreyan.engine.data.Character

sealed abstract class Punctation(val value: String) extends Character {
  val isDouble = false
  override def toString: String = value
}

object Punctation {
  case object DOT extends Punctation(".")
  case object QUESTION extends Punctation("?")
  case object EXCLAIM extends Punctation("!")
  case object DOUBLEQUOTE extends Punctation("\"")
  case object QUOTE extends Punctation("'")
  case object HYPHEN extends Punctation("-")
  case object COMMA extends Punctation(",")
  case object SEMICOLON extends Punctation(";")
  case object COLON extends Punctation(":")
  case object SPACE extends Punctation(" ")
  val values: List[Punctation] = List(DOT, QUESTION, EXCLAIM,
    DOUBLEQUOTE, QUOTE, HYPHEN, COMMA, SEMICOLON, COLON, SPACE)
  val charValues: List[Character] = values.map(_.asInstanceOf[Character])
  def valueOf(v: String): Option[Punctation] = values.find(_.value.equalsIgnoreCase(v))
}

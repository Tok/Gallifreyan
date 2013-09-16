package gallifreyan.engine.characters

import gallifreyan.engine.CircleType
import gallifreyan.engine.MarkType
import gallifreyan.engine.traits.Character

sealed class Consonant(val circleType: CircleType, val markType: MarkType) extends Character {
  val isDouble = this.toString.size > 1
  def doesInterlock(): Boolean = circleType == CircleType.HALF
}

object Consonant {
  case object B extends Consonant(CircleType.OPEN, MarkType.NONE)
  case object CH extends Consonant(CircleType.OPEN, MarkType.DOUBLE_DOT)
  case object D extends Consonant(CircleType.OPEN, MarkType.TRIPPLE_DOT)
  case object F extends Consonant(CircleType.OPEN, MarkType.TRIPPLE_LINE)
  case object G extends Consonant(CircleType.OPEN, MarkType.LINE)
  case object H extends Consonant(CircleType.OPEN, MarkType.DOUBLE_LINE)
  case object J extends Consonant(CircleType.FULL, MarkType.NONE)
  case object K extends Consonant(CircleType.FULL, MarkType.DOUBLE_DOT)
  case object L extends Consonant(CircleType.FULL, MarkType.TRIPPLE_DOT)
  case object M extends Consonant(CircleType.FULL, MarkType.TRIPPLE_LINE)
  case object N extends Consonant(CircleType.FULL, MarkType.LINE)
  case object P extends Consonant(CircleType.FULL, MarkType.DOUBLE_LINE)
  case object T extends Consonant(CircleType.HALF, MarkType.NONE)
  case object SH extends Consonant(CircleType.HALF, MarkType.DOUBLE_DOT)
  case object R extends Consonant(CircleType.HALF, MarkType.TRIPPLE_DOT)
  case object S extends Consonant(CircleType.HALF, MarkType.TRIPPLE_LINE)
  case object V extends Consonant(CircleType.HALF, MarkType.LINE)
  case object W extends Consonant(CircleType.HALF, MarkType.DOUBLE_LINE)
  case object TH extends Consonant(CircleType.STRIKED, MarkType.NONE)
  case object Y extends Consonant(CircleType.STRIKED, MarkType.DOUBLE_DOT)
  case object Z extends Consonant(CircleType.STRIKED, MarkType.TRIPPLE_DOT)
  case object NG extends Consonant(CircleType.STRIKED, MarkType.TRIPPLE_LINE)
  case object QU extends Consonant(CircleType.STRIKED, MarkType.LINE)
  case object X extends Consonant(CircleType.STRIKED, MarkType.DOUBLE_LINE)
  val values: List[Consonant] = List(B, CH, D, F, G, H, J, K, L, M, N, P, T, SH, R, S, V, W, TH, Y, Z, NG, QU, X)
  val charValues: List[Character] = values.map(_.asInstanceOf[Character])
  def valueOf(name: String): Option[Consonant] = values.find(_.toString.equalsIgnoreCase(name))
}

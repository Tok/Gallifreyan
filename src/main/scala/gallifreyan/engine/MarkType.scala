package gallifreyan.engine

sealed abstract class MarkType(val number: Int, val shapeIsDot: Boolean)

object MarkType {
  case object NONE extends MarkType(0, true)
  case object DOUBLE_DOT extends MarkType(2, true)
  case object TRIPPLE_DOT extends MarkType(3, true)
  case object TRIPPLE_LINE extends MarkType(3, false)
  case object LINE extends MarkType(1, false)
  case object DOUBLE_LINE extends MarkType(2, false)
  val values: List[MarkType] = List(NONE, DOUBLE_DOT, TRIPPLE_DOT, TRIPPLE_LINE, LINE, DOUBLE_LINE)
  def valueOf(name: String): Option[MarkType] = values.find(_.toString.equalsIgnoreCase(name))
}

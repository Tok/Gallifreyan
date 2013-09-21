package gallifreyan.shape.parts

import gallifreyan.engine.cases.Circle

case class Dots(val left: Option[Circle], val middle: Option[Circle], val right: Option[Circle]) {
  override def toString: String = {
    left.foreach(_.toString) + "\\" + middle.foreach(_.toString) + "/" + right.foreach(_.toString)
  }
}

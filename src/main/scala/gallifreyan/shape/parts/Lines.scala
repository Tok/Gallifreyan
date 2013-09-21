package gallifreyan.shape.parts

import gallifreyan.engine.cases.Line

case class Lines(val left: Option[Line], val middle: Option[Line], val right: Option[Line]) {
  override def toString: String = {
    left.foreach(_.toString) + "\\" + middle.foreach(_.toString) + "/" + right.foreach(_.toString)
  }
}

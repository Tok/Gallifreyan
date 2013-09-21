package gallifreyan.shape.parts

import gallifreyan.engine.cases.Circle

case class Circles(val inner: Circle, val outer: Option[Circle]) extends Round {
  override def toString: String = {
    "(" + inner.toString + outer.foreach(" " + _.toString) + ")"
  }
}

package gallifreyan.shape.parts

import gallifreyan.engine.cases.Arc

case class Arcs(val inner: Arc, val outer: Option[Arc]) extends Round {
  override def toString: String = {
    "(" + inner.toString + outer.foreach(" " + _.toString) + ")"
  }
}

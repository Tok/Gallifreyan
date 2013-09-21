package gallifreyan.shape

import gallifreyan.shape.parts.Circles
import gallifreyan.shape.parts.Dots
import gallifreyan.shape.parts.Lines
import gallifreyan.engine.cases.Circle

case class PunctationShape(val circles: Option[Circles], val lines: Option[Lines], val dots: Option[Dots])

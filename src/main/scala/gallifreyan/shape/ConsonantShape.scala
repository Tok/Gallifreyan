package gallifreyan.shape

import gallifreyan.shape.parts.Circles
import gallifreyan.shape.parts.Dots
import gallifreyan.shape.parts.Lines
import gallifreyan.shape.parts.Round

case class ConsonantShape(val round: Round, val lines: Option[Lines], val dots: Option[Dots])

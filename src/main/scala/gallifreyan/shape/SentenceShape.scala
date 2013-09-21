package gallifreyan.shape

import gallifreyan.shape.parts.Circles
import gallifreyan.shape.parts.Dots
import gallifreyan.shape.parts.Lines
import gallifreyan.engine.cases.Circle

case class SentenceShape(val inner: Circle, val outer: Circle, val words: List[WordShape], val divots: Option[List[Circle]])

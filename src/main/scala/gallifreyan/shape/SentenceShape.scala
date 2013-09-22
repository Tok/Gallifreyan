package gallifreyan.shape

import gallifreyan.engine.cases.Arc
import gallifreyan.engine.cases.Circle
import gallifreyan.shape.parts.ArcCircle

case class SentenceShape(val inner: Option[ArcCircle], val outer: Circle, val words: List[WordShape], val divots: Option[List[Arc]])

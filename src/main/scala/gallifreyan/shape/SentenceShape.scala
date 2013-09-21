package gallifreyan.shape

import gallifreyan.engine.cases.Arc
import gallifreyan.engine.cases.Circle

case class SentenceShape(val inner: Circle, val outer: Circle, val words: List[WordShape], val divots: Option[List[Arc]])

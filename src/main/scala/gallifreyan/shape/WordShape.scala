package gallifreyan.shape

import gallifreyan.shape.parts.ArcCircle

case class WordShape(val circle: Option[ArcCircle], val syllables: List[SyllableShape], val punctation: Option[PunctationShape])

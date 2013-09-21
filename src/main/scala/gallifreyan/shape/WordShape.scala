package gallifreyan.shape

import gallifreyan.shape.parts.Circles
import gallifreyan.shape.parts.Dots
import gallifreyan.shape.parts.Lines
import gallifreyan.engine.cases.Circle
import gallifreyan.engine.characters.Punctation

case class WordShape(val circle: Circle, val syllables: List[SyllableShape], val punctation: Option[Punctation])

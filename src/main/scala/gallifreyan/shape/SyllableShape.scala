package gallifreyan.shape

import gallifreyan.shape.parts.Lines
import gallifreyan.shape.parts.Circles
import gallifreyan.shape.parts.Dots

case class SyllableShape(val consonant: Option[ConsonantShape], val vowel: Option[VowelShape])

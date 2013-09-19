package gallifreyan.engine.data

import gallifreyan.engine.cases.Circle
import gallifreyan.engine.cases.Coord
import gallifreyan.Size

case class Word(val v: List[Syllable]) {
  if(v.size > 7) { throw new IllegalArgumentException("Words are limited to 7 syllables.") }
  def zipRots: List[(Syllable, Double)] = v.zip((1 to v.size).map(i => 360D - (360D / v.size * i)).reverse.toList)
  def addSyllable(s: Syllable): Word = Word(v ::: List(s))
  def mkString: String = v.mkString
  override def toString: String = v.mkString("_")
}

object Word {
  def circle(): Circle = {
    val center = Coord(Size.width / 2, Size.height / 2)
    val radius = (center.y * 0.7D).intValue
    Circle(center, radius)
  }
}

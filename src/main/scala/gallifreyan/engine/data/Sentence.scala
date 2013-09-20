package gallifreyan.engine.data

import gallifreyan.engine.cases.Circle
import gallifreyan.engine.cases.Coord
import gallifreyan.Size

case class Sentence(val v: List[Word]) {
  if(v.size > 5) { throw new IllegalArgumentException("Sentences are limited to 5 words.") }
  def rots: List[Double] = (1 to v.size).map(i => 360D - (360D / v.size * i)).reverse.toList
  def zipRots: List[(Word, Double)] = v.zip(rots)
  def addWord(w: Word): Sentence = Sentence(v ::: List(w))
  def isSingleWord: Boolean = v.length == 1
  def mkString: String = v.map(_.mkString).mkString(" ")
  override def toString: String = v.mkString("+")
}

object Sentence {
  def circle(): Circle = {
    val center = Coord(Size.width / 2, Size.height / 2)
    val radius = (center.y * 0.9D).intValue
    Circle(center, radius)
  }
  def outer(lineWidth: Int): Circle = circle.addToRadius(lineWidth * 7)
}

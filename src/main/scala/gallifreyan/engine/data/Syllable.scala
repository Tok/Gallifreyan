package gallifreyan.engine.data

case class Syllable(val v: List[Character]) {
  def addChar(c: Character): Syllable = Syllable(v ::: List(c))
  override def toString: String = v.mkString
}

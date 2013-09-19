package gallifreyan.engine.data

case class Word(val v: List[Syllable]) {
  def addSyllable(s: Syllable): Word = Word(v ::: List(s))
  def mkString: String = v.mkString
  override def toString: String = v.mkString("_")
}

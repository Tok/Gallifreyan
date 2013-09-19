package gallifreyan.engine.data

case class Sentence(val v: List[Word]) {
  def addWord(w: Word): Sentence = Sentence(v ::: List(w))
  def mkString: String = v.map(_.mkString).mkString(" ")
  override def toString: String = v.mkString("+")
}

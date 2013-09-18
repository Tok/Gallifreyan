package gallifreyan.util

import org.junit.runner.RunWith
import gallifreyan.AbstractTester
import gallifreyan.engine.characters.Consonant
import gallifreyan.engine.characters.Punctation
import gallifreyan.engine.characters.Vowel
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TextUtilSuite extends AbstractTester {
  test("Uppercase Vowel Characters") {
    assert(TextUtil.getChar("A") === Vowel.A)
    assert(TextUtil.getChar("E") === Vowel.E)
    assert(TextUtil.getChar("I") === Vowel.I)
    assert(TextUtil.getChar("O") === Vowel.O)
    assert(TextUtil.getChar("U") === Vowel.U)
  }

  test("Lowercase Vowel Characters") {
    assert(TextUtil.getChar("a") === Vowel.A)
    assert(TextUtil.getChar("e") === Vowel.E)
    assert(TextUtil.getChar("i") === Vowel.I)
    assert(TextUtil.getChar("o") === Vowel.O)
    assert(TextUtil.getChar("u") === Vowel.U)
  }

  test("Uppercase Consonant Characters") {
    assert(TextUtil.getChar("B") === Consonant.B)
    assert(TextUtil.getChar("D") === Consonant.D)
    assert(TextUtil.getChar("F") === Consonant.F)
    assert(TextUtil.getChar("G") === Consonant.G)
    assert(TextUtil.getChar("H") === Consonant.H)
    assert(TextUtil.getChar("J") === Consonant.J)
    assert(TextUtil.getChar("K") === Consonant.K)
    assert(TextUtil.getChar("L") === Consonant.L)
    assert(TextUtil.getChar("M") === Consonant.M)
    assert(TextUtil.getChar("N") === Consonant.N)
    assert(TextUtil.getChar("P") === Consonant.P)
    assert(TextUtil.getChar("T") === Consonant.T)
    assert(TextUtil.getChar("R") === Consonant.R)
    assert(TextUtil.getChar("S") === Consonant.S)
    assert(TextUtil.getChar("V") === Consonant.V)
    assert(TextUtil.getChar("W") === Consonant.W)
    assert(TextUtil.getChar("Y") === Consonant.Y)
    assert(TextUtil.getChar("Z") === Consonant.Z)
    assert(TextUtil.getChar("X") === Consonant.X)
  }

  test("Lowercase Consonant Characters") {
    assert(TextUtil.getChar("b") === Consonant.B)
    assert(TextUtil.getChar("d") === Consonant.D)
    assert(TextUtil.getChar("f") === Consonant.F)
    assert(TextUtil.getChar("g") === Consonant.G)
    assert(TextUtil.getChar("h") === Consonant.H)
    assert(TextUtil.getChar("j") === Consonant.J)
    assert(TextUtil.getChar("k") === Consonant.K)
    assert(TextUtil.getChar("l") === Consonant.L)
    assert(TextUtil.getChar("m") === Consonant.M)
    assert(TextUtil.getChar("n") === Consonant.N)
    assert(TextUtil.getChar("p") === Consonant.P)
    assert(TextUtil.getChar("t") === Consonant.T)
    assert(TextUtil.getChar("r") === Consonant.R)
    assert(TextUtil.getChar("s") === Consonant.S)
    assert(TextUtil.getChar("v") === Consonant.V)
    assert(TextUtil.getChar("w") === Consonant.W)
    assert(TextUtil.getChar("y") === Consonant.Y)
    assert(TextUtil.getChar("z") === Consonant.Z)
    assert(TextUtil.getChar("x") === Consonant.X)
  }

  test("Uppercase Double Consonants") {
    assert(TextUtil.getChar("CH") === Consonant.CH)
    assert(TextUtil.getChar("SH") === Consonant.SH)
    assert(TextUtil.getChar("TH") === Consonant.TH)
    assert(TextUtil.getChar("NG") === Consonant.NG)
    assert(TextUtil.getChar("QU") === Consonant.QU)
  }

  test("Lowercase Double Consonants") {
    assert(TextUtil.getChar("ch") === Consonant.CH)
    assert(TextUtil.getChar("sh") === Consonant.SH)
    assert(TextUtil.getChar("th") === Consonant.TH)
    assert(TextUtil.getChar("ng") === Consonant.NG)
    assert(TextUtil.getChar("qu") === Consonant.QU)
  }

  test("Mixedcase Double Consonants") {
    assert(TextUtil.getChar("Ch") === Consonant.CH)
    assert(TextUtil.getChar("Sh") === Consonant.SH)
    assert(TextUtil.getChar("Th") === Consonant.TH)
    assert(TextUtil.getChar("Ng") === Consonant.NG)
    assert(TextUtil.getChar("Qu") === Consonant.QU)
    assert(TextUtil.getChar("cH") === Consonant.CH)
    assert(TextUtil.getChar("sH") === Consonant.SH)
    assert(TextUtil.getChar("tH") === Consonant.TH)
    assert(TextUtil.getChar("nG") === Consonant.NG)
    assert(TextUtil.getChar("qU") === Consonant.QU)
  }

  test("Punctation Characters") {
    assert(TextUtil.getChar(".") === Punctation.DOT)
    assert(TextUtil.getChar("?") === Punctation.QUESTION)
    assert(TextUtil.getChar("!") === Punctation.EXCLAIM)
    assert(TextUtil.getChar("\"") === Punctation.DOUBLEQUOTE)
    assert(TextUtil.getChar("'") === Punctation.QUOTE)
    assert(TextUtil.getChar("-") === Punctation.HYPHEN)
    assert(TextUtil.getChar(",") === Punctation.COMMA)
    assert(TextUtil.getChar(";") === Punctation.SEMICOLON)
    assert(TextUtil.getChar(":") === Punctation.COLON)
    assert(TextUtil.getChar(" ") === Punctation.SPACE)
  }

  test("Illegal Characters") {
    assert(TextUtil.getChar("+") === Punctation.SPACE)
    assert(TextUtil.getChar("*") === Punctation.SPACE)
    assert(TextUtil.getChar("=") === Punctation.SPACE)
    assert(TextUtil.getChar("$") === Punctation.SPACE)
    assert(TextUtil.getChar("/") === Punctation.SPACE)
    assert(TextUtil.getChar("\\") === Punctation.SPACE)
    assert(TextUtil.getChar("(") === Punctation.SPACE)
    assert(TextUtil.getChar(")") === Punctation.SPACE)
    assert(TextUtil.getChar("[") === Punctation.SPACE)
    assert(TextUtil.getChar("]") === Punctation.SPACE)
    assert(TextUtil.getChar("{") === Punctation.SPACE)
    assert(TextUtil.getChar("}") === Punctation.SPACE)
    assert(TextUtil.getChar("<") === Punctation.SPACE)
    assert(TextUtil.getChar(">") === Punctation.SPACE)
    assert(TextUtil.getChar("#") === Punctation.SPACE)
    assert(TextUtil.getChar("@") === Punctation.SPACE)
    assert(TextUtil.getChar("&") === Punctation.SPACE)
    assert(TextUtil.getChar("^") === Punctation.SPACE)
    assert(TextUtil.getChar("~") === Punctation.SPACE)
  }

  test("Vowels") {
    val aeiou = "AEIOU"
    val result = TextUtil.getSyllables(aeiou).map(_.mkString).mkString
    assert(result === aeiou)
  }

  test("Consonants") {
    val cons = "BDFGHJKLMNPTRSVWYZX"
    val result = TextUtil.getSyllables(cons).map(_.mkString).mkString
    assert(result === cons)
  }

  test("C to K") {
    val result = TextUtil.getSyllables("C").map(_.mkString).mkString
    assert(result === "K")
  }

  test("Doubles") {
    assert(TextUtil.getSyllables("CH").map(_.mkString).mkString === "CH")
    assert(TextUtil.getSyllables("SH").map(_.mkString).mkString === "SH")
    assert(TextUtil.getSyllables("TH").map(_.mkString).mkString === "TH")
    assert(TextUtil.getSyllables("NG").map(_.mkString).mkString === "NG")
    assert(TextUtil.getSyllables("QU").map(_.mkString).mkString === "QU")
  }

  test("Words") {
    assert(TextUtil.getSyllables("BOW").map(_.mkString).mkString === "BOW")
    assert(TextUtil.getSyllables("TIES").map(_.mkString).mkString === "TIES")
    assert(TextUtil.getSyllables("ARE").map(_.mkString).mkString === "ARE")
    assert(TextUtil.getSyllables("COOL").map(_.mkString).mkString === "KOOL")
  }

  test("Syllable Lists") {
    assert(TextUtil.getSyllables("BOW").map(_.mkString).mkString("_") === "BO_W")
    assert(TextUtil.getSyllables("TIES").map(_.mkString).mkString("_") === "TI_E_S")
    assert(TextUtil.getSyllables("ARE").map(_.mkString).mkString("_") === "A_RE")
    assert(TextUtil.getSyllables("COOL").map(_.mkString).mkString("_") === "KOO_L")
  }

  test("C Relaced With K") {
    assert(TextUtil.getSyllables("CAT").map(_.mkString).mkString("_") === "KA_T")
  }

  test("Q Relaced With QU") {
    assert(TextUtil.getSyllables("FAQ").map(_.mkString).mkString("_") === "FA_QU")
  }

  test("QuU") {
    assert(TextUtil.getSyllables("QuU").map(_.mkString).mkString("_") === "QUU")
  }

  test("C In CH Not Relaced") {
    assert(TextUtil.getSyllables("CHILL").map(_.mkString).mkString("_") === "CHI_LL")
    assert(TextUtil.getSyllables("ORCHID").map(_.mkString).mkString("_") === "O_R_CHI_D")
    assert(TextUtil.getSyllables("BACH").map(_.mkString).mkString("_") === "BA_CH")
  }

  test("Double Consonant Aggregation") {
    assert(TextUtil.getSyllables("ILL").map(_.mkString).mkString("_") === "I_LL")
    assert(TextUtil.getSyllables("MMO").map(_.mkString).mkString("_") === "MMO")
    assert(TextUtil.getSyllables("XX").map(_.mkString).mkString("_") === "XX")
  }

  test("Double Vowel Aggregation") {
    assert(TextUtil.getSyllables("LII").map(_.mkString).mkString("_") === "LII")
    assert(TextUtil.getSyllables("MOO").map(_.mkString).mkString("_") === "MOO")
    assert(TextUtil.getSyllables("AA").map(_.mkString).mkString("_") === "AA")
  }

  test("Different Double Vowel Separation") {
    assert(TextUtil.getSyllables("TIE").map(_.mkString).mkString("_") === "TI_E")
  }
}

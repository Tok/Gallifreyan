package gallifreyan.util

import org.junit.runner.RunWith

import gallifreyan.AbstractTester
import gallifreyan.engine.characters.Consonant
import gallifreyan.engine.characters.Punctation
import gallifreyan.engine.characters.Vowel

class TextUtilSuite extends AbstractTester {
  test("Uppercase Vowel Characters") {
    assert(TextUtil.makeChar("A") === Vowel.A)
    assert(TextUtil.makeChar("E") === Vowel.E)
    assert(TextUtil.makeChar("I") === Vowel.I)
    assert(TextUtil.makeChar("O") === Vowel.O)
    assert(TextUtil.makeChar("U") === Vowel.U)
  }

  test("Lowercase Vowel Characters") {
    assert(TextUtil.makeChar("a") === Vowel.A)
    assert(TextUtil.makeChar("e") === Vowel.E)
    assert(TextUtil.makeChar("i") === Vowel.I)
    assert(TextUtil.makeChar("o") === Vowel.O)
    assert(TextUtil.makeChar("u") === Vowel.U)
  }

  test("Uppercase Consonant Characters") {
    assert(TextUtil.makeChar("B") === Consonant.B)
    assert(TextUtil.makeChar("D") === Consonant.D)
    assert(TextUtil.makeChar("F") === Consonant.F)
    assert(TextUtil.makeChar("G") === Consonant.G)
    assert(TextUtil.makeChar("H") === Consonant.H)
    assert(TextUtil.makeChar("J") === Consonant.J)
    assert(TextUtil.makeChar("K") === Consonant.K)
    assert(TextUtil.makeChar("L") === Consonant.L)
    assert(TextUtil.makeChar("M") === Consonant.M)
    assert(TextUtil.makeChar("N") === Consonant.N)
    assert(TextUtil.makeChar("P") === Consonant.P)
    assert(TextUtil.makeChar("T") === Consonant.T)
    assert(TextUtil.makeChar("R") === Consonant.R)
    assert(TextUtil.makeChar("S") === Consonant.S)
    assert(TextUtil.makeChar("V") === Consonant.V)
    assert(TextUtil.makeChar("W") === Consonant.W)
    assert(TextUtil.makeChar("Y") === Consonant.Y)
    assert(TextUtil.makeChar("Z") === Consonant.Z)
    assert(TextUtil.makeChar("X") === Consonant.X)
  }

  test("Lowercase Consonant Characters") {
    assert(TextUtil.makeChar("b") === Consonant.B)
    assert(TextUtil.makeChar("d") === Consonant.D)
    assert(TextUtil.makeChar("f") === Consonant.F)
    assert(TextUtil.makeChar("g") === Consonant.G)
    assert(TextUtil.makeChar("h") === Consonant.H)
    assert(TextUtil.makeChar("j") === Consonant.J)
    assert(TextUtil.makeChar("k") === Consonant.K)
    assert(TextUtil.makeChar("l") === Consonant.L)
    assert(TextUtil.makeChar("m") === Consonant.M)
    assert(TextUtil.makeChar("n") === Consonant.N)
    assert(TextUtil.makeChar("p") === Consonant.P)
    assert(TextUtil.makeChar("t") === Consonant.T)
    assert(TextUtil.makeChar("r") === Consonant.R)
    assert(TextUtil.makeChar("s") === Consonant.S)
    assert(TextUtil.makeChar("v") === Consonant.V)
    assert(TextUtil.makeChar("w") === Consonant.W)
    assert(TextUtil.makeChar("y") === Consonant.Y)
    assert(TextUtil.makeChar("z") === Consonant.Z)
    assert(TextUtil.makeChar("x") === Consonant.X)
  }

  test("Uppercase Double Consonants") {
    assert(TextUtil.makeChar("CH") === Consonant.CH)
    assert(TextUtil.makeChar("SH") === Consonant.SH)
    assert(TextUtil.makeChar("TH") === Consonant.TH)
    assert(TextUtil.makeChar("NG") === Consonant.NG)
    assert(TextUtil.makeChar("QU") === Consonant.QU)
  }

  test("Lowercase Double Consonants") {
    assert(TextUtil.makeChar("ch") === Consonant.CH)
    assert(TextUtil.makeChar("sh") === Consonant.SH)
    assert(TextUtil.makeChar("th") === Consonant.TH)
    assert(TextUtil.makeChar("ng") === Consonant.NG)
    assert(TextUtil.makeChar("qu") === Consonant.QU)
  }

  test("Mixedcase Double Consonants") {
    assert(TextUtil.makeChar("Ch") === Consonant.CH)
    assert(TextUtil.makeChar("Sh") === Consonant.SH)
    assert(TextUtil.makeChar("Th") === Consonant.TH)
    assert(TextUtil.makeChar("Ng") === Consonant.NG)
    assert(TextUtil.makeChar("Qu") === Consonant.QU)
    assert(TextUtil.makeChar("cH") === Consonant.CH)
    assert(TextUtil.makeChar("sH") === Consonant.SH)
    assert(TextUtil.makeChar("tH") === Consonant.TH)
    assert(TextUtil.makeChar("nG") === Consonant.NG)
    assert(TextUtil.makeChar("qU") === Consonant.QU)
  }

  test("Punctation Characters") {
    assert(TextUtil.makeChar(".") === Punctation.DOT)
    assert(TextUtil.makeChar("?") === Punctation.QUESTION)
    assert(TextUtil.makeChar("!") === Punctation.EXCLAIM)
    assert(TextUtil.makeChar("\"") === Punctation.DOUBLEQUOTE)
    assert(TextUtil.makeChar("'") === Punctation.QUOTE)
    assert(TextUtil.makeChar("-") === Punctation.HYPHEN)
    assert(TextUtil.makeChar(",") === Punctation.COMMA)
    assert(TextUtil.makeChar(";") === Punctation.SEMICOLON)
    assert(TextUtil.makeChar(":") === Punctation.COLON)
    assert(TextUtil.makeChar(" ") === Punctation.SPACE)
  }

  test("Illegal Characters") {
    assert(TextUtil.makeChar("+") === Punctation.SPACE)
    assert(TextUtil.makeChar("*") === Punctation.SPACE)
    assert(TextUtil.makeChar("=") === Punctation.SPACE)
    assert(TextUtil.makeChar("$") === Punctation.SPACE)
    assert(TextUtil.makeChar("/") === Punctation.SPACE)
    assert(TextUtil.makeChar("\\") === Punctation.SPACE)
    assert(TextUtil.makeChar("(") === Punctation.SPACE)
    assert(TextUtil.makeChar(")") === Punctation.SPACE)
    assert(TextUtil.makeChar("[") === Punctation.SPACE)
    assert(TextUtil.makeChar("]") === Punctation.SPACE)
    assert(TextUtil.makeChar("{") === Punctation.SPACE)
    assert(TextUtil.makeChar("}") === Punctation.SPACE)
    assert(TextUtil.makeChar("<") === Punctation.SPACE)
    assert(TextUtil.makeChar(">") === Punctation.SPACE)
    assert(TextUtil.makeChar("#") === Punctation.SPACE)
    assert(TextUtil.makeChar("@") === Punctation.SPACE)
    assert(TextUtil.makeChar("&") === Punctation.SPACE)
    assert(TextUtil.makeChar("^") === Punctation.SPACE)
    assert(TextUtil.makeChar("~") === Punctation.SPACE)
  }

  test("Separated Vowels") {
    val aeiou = "AEIOU"
    val result = TextUtil.makeWord(aeiou).toString
    assert(result === "A_E_I_O_U")
  }

  test("Aggregated Vowels") {
    val aeiou = "AAEEII"
    val result = TextUtil.makeWord(aeiou).toString
    assert(result === "AA_EE_II")    
  }

  test("Consonants") {
    assert(TextUtil.makeWord("BDFGHJK").toString === "B_D_F_G_H_J_K")
    assert(TextUtil.makeWord("LMNPTRS").toString === "L_M_N_P_T_R_S")
    assert(TextUtil.makeWord("VWYZX").toString === "V_W_Y_Z_X")
  }

  test("C to K") {
    val result = TextUtil.makeWord("C").toString
    assert(result === "K")
  }

  test("Doubles") {
    assert(TextUtil.makeWord("CH").toString === "CH")
    assert(TextUtil.makeWord("SH").toString === "SH")
    assert(TextUtil.makeWord("TH").toString === "TH")
    assert(TextUtil.makeWord("NG").toString === "NG")
    assert(TextUtil.makeWord("QU").toString === "QU")
  }

  test("Words") {
    assert(TextUtil.makeWord("BOW").toString === "BO_W")
    assert(TextUtil.makeWord("TIES").toString === "TI_E_S")
    assert(TextUtil.makeWord("ARE").toString === "A_RE")
    assert(TextUtil.makeWord("COOL").toString === "KOO_L")
  }

  test("C Relaced With K") {
    assert(TextUtil.makeWord("CAT").toString === "KA_T")
  }

  test("Q Relaced With QU") {
    assert(TextUtil.makeWord("FAQ").toString === "FA_QU")
  }

  test("QuU") {
    assert(TextUtil.makeWord("QuU").toString === "QUU")
  }

  test("C In CH Not Relaced") {
    assert(TextUtil.makeWord("CHILL").toString === "CHI_LL")
    assert(TextUtil.makeWord("ORCHID").toString === "O_R_CHI_D")
    assert(TextUtil.makeWord("BACH").toString === "BA_CH")
  }

  test("Double Consonant Aggregation") {
    assert(TextUtil.makeWord("ILL").toString === "I_LL")
    assert(TextUtil.makeWord("MMO").toString === "MMO")
    assert(TextUtil.makeWord("XX").toString === "XX")
  }

  test("Double Vowel Aggregation") {
    assert(TextUtil.makeWord("LII").toString === "LII")
    assert(TextUtil.makeWord("MOO").toString === "MOO")
    assert(TextUtil.makeWord("AA").toString === "AA")
  }

  test("Different Double Vowel Separation") {
    assert(TextUtil.makeWord("TIE").toString === "TI_E")
  }

  test("Word String") {
    val word = TextUtil.makeWord("Word")
    assert(word.mkString === "WORD")
  }

  test("Sentence") {
    val sentence = TextUtil.makeSentence("Test Test")
    assert(sentence.toString === "TE_S_T+TE_S_T")
  }
  
  test("Sentence String") {
    val sentence = TextUtil.makeSentence("Test Test")
    assert(sentence.mkString === "TEST TEST")
  }

  test("Word Limit For Sentence") {
    val inLimit = TextUtil.makeSentence("sha sha sha sha sha")
    val e = intercept[IllegalArgumentException] { TextUtil.makeSentence("sha sha sha sha sha sha") }
    assert(e.isInstanceOf[IllegalArgumentException])
  }

  test("Syllable Limit For Word") {
    val inLimit = TextUtil.makeWord("shashashashashashasha")
    val e = intercept[IllegalArgumentException] { TextUtil.makeWord("shashashashashashashasha") }
    assert(e.isInstanceOf[IllegalArgumentException])
  }
}

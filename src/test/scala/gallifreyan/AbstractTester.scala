package gallifreyan

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import org.scalatest.BeforeAndAfter
import org.scalatest.FunSuite
import org.scalatest.mock.EasyMockSugar
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
abstract class AbstractTester extends FunSuite with EasyMockSugar with BeforeAndAfter {
  val printStream = System.err

  before {
    System.setErr(new PrintStream(new ByteArrayOutputStream))
  }

  after {
    System.setErr(printStream)
  }

  def testAny(a: Any, executeProductTests: Boolean): Unit = {
    val p = a.asInstanceOf[Product]
    assert(p.productIterator.isInstanceOf[Iterator[Any]])
    if (executeProductTests) {
      val e = intercept[IndexOutOfBoundsException] { p.productElement(0) }
      assert(e.isInstanceOf[IndexOutOfBoundsException])
      assert(p.productPrefix === a.toString)
      assert(p.productArity === 0)
    }
    val eq = a.asInstanceOf[Equals]
    assert(!eq.canEqual(new Object))
  }
}

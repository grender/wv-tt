import com.github.grender.wv_tt.Balance
import org.scalatest.FlatSpec

class BalanceTest extends FlatSpec {
  "Balance" should "created with balance 0" in {
    assert((new Balance).get==0)
  }

  it should "add/reduce correct" in {
    val b = new Balance

    b.add(10)
    assert(b.get==10)

    b.reduce(5)
    assert(b.get==5)

    val exception = intercept[Exception] {
      b.reduce(6)
    }
    assert(exception.getMessage.startsWith("Can't reduce to value"))
  }

}

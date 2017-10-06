
import org.scalatest._

class SweeperSpec extends FlatSpec with Matchers {
  "the sweeper" should "return false" in {
    Sweeper.sweep() shouldBe true
  }
}

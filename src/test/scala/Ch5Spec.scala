package swscala.unit

import org.scalacheck.Arbitrary
import org.scalatest._
import swscala._

class Ch5Spec extends FlatSpec with Matchers with CatsLawChecking {
  import Ch5._

  it should "Problem 1" in {
    import Problem1._

    isLong[Long] shouldEqual true
    isLong[Double] shouldEqual true
    isLong[Int] shouldEqual false
    isLong[Short] shouldEqual false
    isLong[Float] shouldEqual false
    "isLong[String]" shouldNot compile
  }

  it should "Problem 2" in {
    import Problem2._

    // Check the laws
    checkCatsMonoidLaws[Data](_ shouldEqual _)(implicitly[Arbitrary[Data]], monoidInstance)
  }


}

package swscala.unit

import cats.Monoid
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
    checkCatsMonoidLaws[Data]()(implicitly[Arbitrary[Data]], monoidInstance)
  }

  it should "Problem 3" in {
    import Problem3._

    // Int monoid
    implicit val monoidIntInstance = new Monoid[Int] {
      override def empty = 0
      override def combine(x: Int, y: Int) = x + y
    }

    def dataIsEqual[R, A](d1: R => A, d2: R => A)(implicit arbR: Arbitrary[R]): Assertion = {
      forAll { (r: R) => d1(r) shouldEqual d2(r) }
    }

    // Check monoid laws for R => A
    checkCatsMonoidLaws[Boolean => Int](dataIsEqual[Boolean, Int])
  }


}

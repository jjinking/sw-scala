package swscala.unit

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import cats.{Monoid, Semigroup}
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

  it should "Problem 4" in {
    import Problem4._

    // Create instance of semigroup S.
    // Taken from https://github.com/winitzki/scala-examples/blob/master/chapter05/src/test/scala/example/Chapter05_03_workedExamplesSpec.scala#L138
    implicit val semigroupIntInstanceNonCommutAssoc = new Semigroup[Int] {
      override def combine(x: Int, y: Int): Int = nonCommutAssoc(x, y)

      // Budden's function: see F. J. Budden, A Non-Commutative, Associative Operation on the Reals.
      //   The Mathematical Gazette, Vol. 54, No. 390 (Dec., 1970), pp. 368-372
      private def nonCommutAssoc(x: Int, y: Int): Int =
        if (x % 2 == 0) x + y else x - y
    }

    // Should be able to derive monoid instance for (Int, Double) now
    checkCatsMonoidLaws[Option[Int]]()
  }

  it should "Problem 5" in {
    import Problem5._
    import scala.concurrent.ExecutionContext.Implicits.global

    def dataIsEqual[T](f1: Future[Seq[T]], f2: Future[Seq[T]])(implicit arbT: Arbitrary[T]): Assertion = {
      Await.result(f1, 1 second) shouldEqual Await.result(f2, 1 second)
      // for {
      //   r1 <- f1
      //   r2 <- f2
      // } yield r1 shouldEqual r2
    }

    checkCatsFunctorLaws[F, Int, String, Boolean](dataIsEqual)
  }

}

package swscala.unit

import org.scalacheck.ScalacheckShapeless._
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import swscala._


class Ch4Spec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "Ch4Ex1" should "pass all tests" in {
    import Ch4Ex1._

    // Problem 1

    // Identity law.
    forAll { (x: P1Data[Int]) ⇒ p1Fmap(identity[Int])(x) shouldEqual x }

    // Composition law.
    forAll { (x: P1Data[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒
      p1Fmap(f andThen g)(x) shouldEqual (p1Fmap(f) andThen p1Fmap(g)) (x) }


    // Problem 2
    def p2DataEqual[A](d1: P2Data[A], d2: P2Data[A]) = {
      val aToString: A => String = a => a.toString
      val (a1, iOrA1) = d1.d(aToString)
      val (a2, iOrA2) = d2.d(aToString)
      a1 shouldEqual a2
      iOrA1 shouldEqual iOrA2
    }

    // Identity law.
    forAll { (x: P2Data[Int]) ⇒ p2DataEqual(p2Fmap(identity[Int])(x), x) }

    // Composition law.
    forAll { (x: P2Data[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒
      p2DataEqual(
        p2Fmap(f andThen g)(x),
        (p2Fmap(f) andThen p2Fmap(g))(x)
      )
    }


  }

}

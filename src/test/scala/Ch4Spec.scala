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
    "def p2Fmap[A, B](f: A => B): P2Data[A] => P2Data[B] = implement" shouldNot typeCheck


  }

}

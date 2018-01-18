package swscala.unit

import org.scalacheck.Arbitrary
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
    def p2DataEqual[A](
      d1: P2Data[A], d2: P2Data[A]
    )(implicit arbAToStr: Arbitrary[A => String]) = {
      forAll { (aToString: A => String) =>
        val (a1, iOrA1) = d1.d(aToString)
        val (a2, iOrA2) = d2.d(aToString)
        a1 shouldEqual a2
        iOrA1 shouldEqual iOrA2
      }
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


    // Problem 3
    def p3DataEqual[A, B](
      d1: P3Data[A, B], d2: P3Data[A, B]
    )(implicit arbA: Arbitrary[A], arbAOrB: Arbitrary[Either[A, B]]) = {
      forAll { (a: A, aOrB: Either[A, B]) =>
        val (aToString1, aOrBToInt1) = d1
        val (aToString2, aOrBToInt2) = d2
        aToString1(a) shouldEqual aToString2(a)
        aOrBToInt1(aOrB) shouldEqual aOrBToInt2(aOrB)
      }
    }

    // Identity Law for B => C
    forAll { (x: P3Data[Int, String]) ⇒ p3DataEqual(p3ContraFmapBC(identity[String])(x), x) }

    // Composition Law for B => C
    forAll { (x: P3Data[Boolean, Int], f: String ⇒ Int, g: Long ⇒ String) ⇒
      p3DataEqual(
        p3ContraFmapBC(g andThen f)(x),
        (p3ContraFmapBC[Boolean, Int, String](f) andThen p3ContraFmapBC(g))(x)
      )
    }

    // Identity Law for A => C
    forAll { (x: P3Data[String, Int]) ⇒ p3DataEqual(p3ContraFmapAC(identity[String])(x), x) }

    // Composition Law for A => C
    forAll { (x: P3Data[Int, Boolean], f: String ⇒ Int, g: Long ⇒ String) ⇒
      p3DataEqual(
        p3ContraFmapAC(g andThen f)(x),
        (p3ContraFmapAC[Int, Boolean, String](f) andThen p3ContraFmapAC(g))(x)
      )
    }

    // Problem 5
    def p5DataEqual[B](d1: P5Data[B], d2: P5Data[B]) = {
      val (bOrIntToB1, bOrStrToB1) = d1
      val (bOrIntToB2, bOrStrToB2) = d2

      forAll { (i: Int) =>
        bOrIntToB1 match {
          case Left(b1) => bOrIntToB2 match {
            case Left(b2) => b1 shouldEqual b2
            case Right(_) => fail
          }
          case Right(intToB1) => bOrIntToB2 match {
            case Left(_) => fail
            case Right(intToB2) => intToB1(i) shouldEqual intToB2(i)
          }
        }
      }

      forAll { (s: String) =>
        bOrStrToB1 match {
          case Left(b1) => bOrStrToB2 match {
            case Left(b2) => b1 shouldEqual b2
            case Right(_) => fail
          }
          case Right(strToB1) => bOrStrToB2 match {
            case Left(_) => fail
            case Right(strToB2) => strToB1(s) shouldEqual strToB2(s)
          }
        }
      }
    }

    // Identity Law
    forAll { (x: P5Data[String]) ⇒ p5DataEqual(p5Fmap(identity[String])(x), x) }

    // Composition Law
    forAll { (x: P5Data[Boolean], f: Boolean ⇒ Int, g: Int ⇒ String) ⇒
      p5DataEqual(
        p5Fmap(f andThen g)(x),
        (p5Fmap(f) andThen p5Fmap(g))(x)
      )
    }



  }

}

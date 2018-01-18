package swscala

import scala.reflect.ClassTag


object Ch4Ex1 {

  // Problem 1
  // Data[A] ≡ (1 + A) × (1 + A) × String
  // Covariant
  final case class P1Data[A](d1: Option[A], d2: Option[A], d3: String)

  def p1Fmap[A, B](f: A => B): P1Data[A] => P1Data[B] = { p1DA =>
    P1Data(p1DA.d1.map(f), p1DA.d2.map(f), p1DA.d3)
  }


  // Problem 2
  // Data[A] ≡ (A ⇒ String) ⇒ (A × (Int + A))
  // Covariant
  final case class P2Data[A](d: (A => String) => (A, Either[Int, A]))

  def p2Fmap[A, B](f: A => B): P2Data[A] => P2Data[B] = { p2DA =>
    val dB: (B => String) => (B, Either[Int, B]) = { bToString =>
      val aToString: A => String = a => bToString(f(a))
      val (a: A, intOrA: Either[Int, A]) = p2DA.d(aToString)
      val b: B = f(a)
      val iOrB: Either[Int, B] = intOrA match {
        case Left(i) => Left(i)
        case Right(a) => Right(f(a))
      }
      (b, iOrB)
    }
    P2Data(dB)
  }


  // Problem 3
  // Data[A, B] ≡ (A ⇒ String) × ((A + B) ⇒ Int)
  // Contravariant
  type P3Data[A, B] = ((A ⇒ String), (Either[A, B] ⇒ Int))

  // B => C
  def p3ContraFmapBC[A, B, C](f: C => B): P3Data[A, B] => P3Data[A, C] = { p3DAB =>
    val (aToString, aOrBToInt) = p3DAB
    val aOrCToInt: Either[A, C] => Int = {
      case Left(a) => aOrBToInt(Left(a))
      case Right(c) => aOrBToInt(Right(f(c)))
    }
    (aToString, aOrCToInt)
  }

  // A => C
  def p3ContraFmapAC[A, B, C](f: C => A): P3Data[A, B] => P3Data[C, B] = { p3DAB =>
    val (aToString, aOrBToInt) = p3DAB
    val cToString: C => String = c => aToString(f(c))
    val cOrBToInt: Either[C, B] => Int = {
      case Left(c) => aOrBToInt(Left(f(c)))
      case Right(b) => aOrBToInt(Right(b))
    }
    (cToString, cOrBToInt)
  }


  // Problem 4
  // Data[A] ≡ (1 + (A ⇒ String)) ⇒ (1 + (A ⇒ Int)) ⇒ Int
  // Neither covariant nor contravariant
  // type P4Data[A] = Some[A ⇒ String] ⇒ Some[A ⇒ Int] ⇒ Int

  // The following will fail:
  // import io.chymyst.ch._
  // def p4Fmap[A, B](f: A => B): P4Data[A] => P4Data[B] = implement
  // def p4ContraFmap[A, B](f: B => A): P4Data[A] => P4Data[B] = implement


  // Problem 5
  // Data[B] ≡ (B + (Int ⇒ B)) × (B + (String ⇒ B))
  // Covariant
  type P5Data[B] = (Either[B, Int => B], Either[B, String => B])

  def p5Fmap[B, C](f: B => C): P5Data[B] => P5Data[C] = { p5DB =>
    val (bOrIntToB, bOrStrToB) = p5DB
    val cOrIntToC: Either[C, Int => C] = bOrIntToB match {
      case Left(b) => Left(f(b))
      case Right(intToB) => Right(i => f(intToB(i)))
    }
    val cOrStrToC: Either[C, String => C] = bOrStrToB match {
      case Left(b) => Left(f(b))
      case Right(strToB) => Right(s => f(strToB(s)))
    }
    (cOrIntToC, cOrStrToC)
  }

}

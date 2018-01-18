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


}

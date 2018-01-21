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
  // Covariant
  // The following will pass:
  // final case class P4Data[A](d: Option[A ⇒ String] ⇒ Option[A ⇒ Int] ⇒ Int)
  // import io.chymyst.ch._
  // def p4Fmap[A, B](f: A => B): P4Data[A] => P4Data[B] = implement
  // not pass: def p4ContraFmap[A, B](f: B => A): P4Data[A] => P4Data[B] = implement
  type P4Data[A] = Option[A => String] => Option[A => Int] => Int
  def p4Fmap[A, B](f: A => B): P4Data[A] => P4Data[B] = { p4DataA =>
    // p4DataA: Option[A => String] => Option[A => Int] => Int

    val p4DataB: Option[B ⇒ String] ⇒ Option[B ⇒ Int] ⇒ Int = { optBToString => optBToInt =>
      // optBToString: Option[B => String]
      // optBToInt: Option[B => Int]

      // First produce Option[A => Int] => Int using p4DataA and optBToString
      val optToIntA: Option[A => Int] => Int = optBToString match {
        case Some(bToStr) => {
          val aToStr: A => String = a => bToStr(f(a))
          p4DataA(Some(aToStr))
        }
        case None => {
          p4DataA(None)
        }
      }

      // Second produce Int from optBToInt using optToIntA and optBToInt
      val i2: Int = optBToInt match {
        case Some(bToInt) => {
          val aToInt: A => Int = a => bToInt(f(a))
          optToIntA(Some(aToInt))
        }
        case None => optToIntA(None)
      }
      // Must return integer
      i2
    }
    p4DataB
  }


  // Problem 5
  // Data[B] ≡ (B + (Int ⇒ B)) × (B + (String ⇒ B))
  // Covariant
  type P5Data[B] = (BOrXToB[B, Int], BOrXToB[B, String])

  // Sub-structure is covariant wrt B since it's not consumed
  type BOrXToB[B, X] = Either[B, X => B]

  def fmapBOrXToB[B, C, X](f: B => C): BOrXToB[B, X] => BOrXToB[C, X] = {
    case Left(b) => Left(f(b))
    case Right(xToB) => Right(x => f(xToB(x)))
  }

  def p5Fmap[B, C](f: B => C): P5Data[B] => P5Data[C] = { p5DB =>
    val (bOrIntToB, bOrStrToB) = p5DB
    (fmapBOrXToB(f)(bOrIntToB), fmapBOrXToB(f)(bOrStrToB))
  }


  // Problem 6
  // Short notation:
  // Data[A, B] ≡ ((A x B) x (B => Int)) + (A x B x Int) + ((String => A) x (B => A))
  // A is never consumed, so it is covariant
  // B behaves like a functor in case class `Re` but is consumed in the others,
  // so it's invariant
  sealed trait Coi[+A, B]
  case class Pa[+A, B](b: (A, B), c: B⇒Int) extends Coi[A, B]
  case class Re[+A, B](d: A, e: B, c: Int) extends Coi[A, B]
  case class Ci[+A, B](f: String⇒A, g: B⇒A) extends Coi[A, B]

}

package swscala


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
  // A is both consumed and produced, so it's neither functor nor contrafunctor
  // fmap and contrafmap can be implemented
  final case class P2Data[A](d: (A => String) => (A, Either[Int, A]))


  // import io.chymyst.ch._
  // def p2Fmap[A, B](f: A => B): P2Data[A] => P2Data[B] = implement

  def p2Fmap[A, B](f: A => B): P2Data[A] => P2Data[B] = { p2DA =>
    // p2DA.d: (A => String) => (A, Either[Int, A])
    def dB: (B => String) => (B, Either[Int, B]) = { bToString =>

      p2DA.d

      val a: A = 
      val b: B = f(a)

      val iOrB = 
      (b, iOrB)
    }
    P2Data(dB)
  }

}

package swscala

import cats.Monoid

object Ch5 {

  object Problem1 {
    final case class LongBits[T](b: Boolean)

    implicit val longBitsLongEv: LongBits[Long] = LongBits(true)
    implicit val longBitsDoubleEv: LongBits[Double] = LongBits(true)
    implicit val longBitsIntEv: LongBits[Int] = LongBits(false)
    implicit val longBitsShortEv: LongBits[Short] = LongBits(false)
    implicit val longBitsFloatEv: LongBits[Float] = LongBits(false)

    def isLong[T](implicit ev: LongBits[T]): Boolean = ev.b
  }

  object Problem2 {
    // Define a monoid instance for the type String × (1 + Int)
    type Data = (String, Option[Int])

    val monoidInstance = new Monoid[Data] {
      override def empty: Data = ("", None)

      override def combine(x: Data, y: Data): Data = x match {
        case (s1, Some(i1)) => y match {
          case (s2, Some(i2)) => (s1 + s2, Some(i1 + i2))
          case (s2, None) => (s1 + s2, Some(i1))
        }
        case (s1, None) => y match {
          case (s2, Some(i2)) => (s1 + s2, Some(i2))
          case (s2, None) => (s1 + s2, None)
        }
      }
    }
  }

  object Problem3 {
    // If A is a monoid and R any type, define monoid instance for R ⇒ A
    implicit def monoidRToAInstance[R, A](
      implicit evA: Monoid[A]
    ): Monoid[R => A] = new Monoid[R => A] {

      override def empty: R => A = r => evA.empty

      override def combine(x: R => A, y: R => A): R => A = r => evA.combine(x(r), y(r))
    }
  }



}

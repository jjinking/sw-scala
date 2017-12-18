package swscala


object Ch3Ex1 {

  /**
    Problem 1

    id(id)
    def id[T]: (T => T) = t => t
    can be rewritten as
    id[T](t: T): T = t
    pass in argument id[U]: U => U
    id[T](id[U]: U => U): T
    substitute T with (U => U)
    id[U => U](id[U]: U => U): U => U
    combine id(id) to idid
    idid[U] = U => U
    val idid[A] = id[A => A](id[A])
    idid does the same as what id[A] does

    id(const)
    def const[C, X]: (C ⇒ X ⇒ C) = c ⇒ x ⇒ c
    id[T](const[C, X]: C => X => C): T
    substitute T with C => X => C
    id[C => X => C](const[C, X]: C => X => C): C => X => C

    id(id)(id)
    substitute id(id) with idid
    idid(id)
    Using same logic to get from id(id) to idid,
    ididid[V] = V => V

    id((id(id))) works, using simliar logic as above
    */
  def id[T]: (T => T) = t => t

  /**
    Problem 2

    const(const)
    def const[C, X]: C ⇒ X ⇒ C = c ⇒ x ⇒ c
    can be rewritten as
    def const[C, X](c: C): X => C = x => c
    const[C, X](const[D, Y]: (D => Y => D)): X => C
    substitute D => Y => D into C
    const[D => Y => D, X](const[D, Y]): X => D => Y => D
    const(const) returns a function that takes in an argument of type X and returns const[D, Y]
    */
  def const[C, X]: C ⇒ X ⇒ C = c ⇒ x ⇒ c

  /**
    Problem 3

    twice(twice(twice))) returns a function take takes in an argument x and apply f 16 times
  
    twice[T](f: T => T): T => T
    twice[T]: (T => T) => T => T
    twice[T](twice[U]: (U => U) => U => U): T => T
    Substitute U => U for T
    twice[U => U](twice[U]): (U => U) => U => U
    twicetwice = twice(twice)
    twicetwice[U]: (U => U) => U => U
    Similarly we can derive
    twicetwicetwice[U]: (U => U) => U => U
    each wrap of twice basically squares the previous - not intuitive?
    */
  def twice[T](f: T => T): T => T = x => f(f(x))

  /**
    Problem 4

    thrice3 = thrice(thrice(thrice)) takes as argument a function f, and returns
    a function that applies f 3 ^ (3 ^ 3) times to an input argument (^ indicates exponent)
    */
  def thrice[T](f: T => T): T => T = x => f(f(f(x)))

  // Problem 5
  def ence[T](f: T => T, n: Int): T => T = {
    if (n == 0) x => x
    else if (n == 1) f
    else x => ence(f, n - 1)(f(x))
  }

  // Problem 6
  def swapFunc[A, B, C](f: (A, B) => C): (B, A) => C = (b: B, a: A) => f(a, b)

  /**
    Problem 7
    Infer types
    def r[...]:... = p ⇒ q ⇒ p(t ⇒ t(q))
    let q: A, t: A => B, p: ((A => B) => B) => C
    def r[...]:... = (p: ((A => B) => B) => C) => (q: A) => (p(t => t(q)): C)
    r[A, B, C]: (((A => B) => B) => C) => A => C
    */

  /**
    Problem 8
    Show not well typed
    def r[...]:... = p ⇒ p(q ⇒ q(p))
    let p: A and q: A => B
    def r[...]:... = p: A ⇒ p(q:(A => B) ⇒ q(p: A): B)
    then p: ((A => B) => B) => C
    but p: A, which cannot equal the above
    Cannot infer types for this function
    */

  /**
    Problem 9
    Infer types
    def s[...]:... = f ⇒ g ⇒ g(x ⇒ x(f(g)))
    let g: A => B, f: (A => B) => C, x: C => D
    def s[...]:... = (f: (A => B) => C) ⇒ (g: A => B) ⇒ g((x: C => D) ⇒ x(f(g):C):D)
    A = (C => D) => D
    def s[...]:... = (f: (((C => D) => D) => B) => C) ⇒ (g: ((C => D) => D) => B) ⇒ g((x: C => D) ⇒ x(f(g):C):D)
    s[B, C, D]: ((((C => D) => D) => B) => C) => (((C => D) => D) => B) => B
    */

  /**
    Problem 10
    Show not well typed
    def s[...]:... = f ⇒ g ⇒ g(x ⇒ f(g(x)))
    let x: A, g: A => B, f: B => C
    def s[...]:... = (f: B => C) ⇒ (g: A => B) ⇒ g(x: A ⇒ f(g(x:A):B):C)
    g: A => C contradicts with A => B, therefore not well typed
    */
}

object Ch3Ex2 {

  // Problem 1
  sealed trait CellState {
    def isBomb: Boolean = this match {
      case BombCell() => true
      case _ => false
    }
  }

  final case class ClosedCell() extends CellState
  final case class BombCell() extends CellState
  final case class OpenCell(neighborBombs: Int) extends CellState

  // Problem 2
  def numCellsShowingZeroNeighborBombs(cells: Seq[Seq[CellState]]): Int = {
    cells.flatten.count {
      _ match {
        case OpenCell(0) => true
        case _ => false
      }
    }
  }

  // Problem 3
  sealed trait RootOfLinear
  final case class NoRoot() extends RootOfLinear
  final case class OneRoot(x: Double) extends RootOfLinear
  final case class AllXRoots() extends RootOfLinear
  def solve1(a: Double, b: Double): RootOfLinear = (a, b) match {
    case (0, 0) => AllXRoots()
    case (0, _) => NoRoot()
    case (a, b) => OneRoot(-b / a)
  }

  // Problem 4
  def solve1(pairs: Seq[(Double, Double)]): Seq[Double] = {
    pairs.map{ case (a, b) => solve1(a, b) }.flatMap {
      _ match {
        case OneRoot(x) => Some(x)
        case _ => None
      }
    }
  }

  // Problem 5
  def f1[A, B](ab: Option[(A, B)]): (Option[A], Option[B]) =
    ab.map{case (a, b) => (Some(a), Some(b))}.getOrElse((None, None))

  def f2[A, B](aXorB: Either[A,B]): (Option[A], Option[B]) = aXorB match {
    case Left(a) => (Some(a), None)
    case Right(b) => (None, Some(b))
  }

  def f3[A,B,C](abc: Either[A, Either[B,C]]): Either[Either[A,B], C] = abc match {
    case Left(a) => Left(Left(a))
    case Right(Left(b)) => Left(Right(b))
    case Right(Right(c)) => Right(c)
  }

}

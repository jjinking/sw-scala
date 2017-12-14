package swscala


object Ch3Exercises {

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
    def r[...]:... = p ⇒ p(q ⇒ q(p))
    let p: A and q: A => B
    def r[...]:... = p: A ⇒ p(q:(A => B) ⇒ q(p: A): B)
    then p: ((A => B) => B) => C
    but p: A, which cannot equal the above
    Cannot infer types for this function
    */

  /**
    Problem 8
    Infer types
    def s[...]:... = f ⇒ g ⇒ g(x ⇒ x(f(g)))
    let g: A => B, f: (A => B) => C, x: C => D
    def s[...]:... = (f: (A => B) => C) ⇒ (g: A => B) ⇒ g((x: C => D) ⇒ x(f(g):C):D)
    A = (C => D) => D
    def s[...]:... = (f: (((C => D) => D) => B) => C) ⇒ (g: ((C => D) => D) => B) ⇒ g((x: C => D) ⇒ x(f(g):C):D)
    s[B, C, D]: ((((C => D) => D) => B) => C) => (((C => D) => D) => B) => B
    */

  /**
    Problem 9
    Show not well typed
    def s[...]:... = f ⇒ g ⇒ g(x ⇒ f(g(x)))
    let x: A, g: A => B, f: B => C
    def s[...]:... = (f: B => C) ⇒ (g: A => B) ⇒ g(x: A ⇒ f(g(x:A):B):C)
    g: A => C contradicts with A => B, therefore not well typed
    */
}

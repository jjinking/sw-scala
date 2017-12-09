package swscala


object Exercises1 {

  val zeroToNine = (0 to 9)

  // Problem 1
  lazy val p1: Seq[(Int, Int)] = zeroToNine.flatMap(i => zeroToNine.map(j => (i, j)))
    .filter{case (i, j) => i + 4 * j  > i * j}

  // Problem 2
  def p2(ss: Seq[String], bs: Seq[Boolean]): Seq[String] =
    ss.zip(bs).filter(_._2).map(_._1)

  // Problem 3
  def p3(nums: Seq[Int]): Seq[(Int, Boolean)] =
    nums.zip(nums.zip(nums.tail).map{case (former: Int, latter: Int) => former < latter})

  // Problem 4
  def p4[S, I](a: Seq[S], b: Seq[I]): Map[I, S] = b.zip(a).toMap

  // Problem 5
  def p5[S](a: Seq[S], b: Seq[Int]) = a.zip(b).sortBy(_._2).map(_._1)

}

object Exercises2 {

  // Problem 1
  def p1(purchasedItems: Seq[(String, Int)]): Map[String, Int] =
    purchasedItems.groupBy(_._1).map{case (k, v) => (k, v.map(_._2).sum)}

  // Problem 2
  def p2(numsLists: Seq[List[Int]]): Seq[List[Int]] =
    numsLists.map(_.sortBy(-_).take(3))

  // Problem 3
  def p3[I, J](a: Set[I], b: Set[J]): Set[(I, J)] =
    a.flatMap(x => b.map(y => (x, y)))

  // Problem 4
  def p4[Person, Amount](daily: Seq[Map[Person, Amount]]): Map[Person, Seq[Amount]] =
    daily.flatMap(_.toSeq).groupBy(_._1).map{case (k, v) => (k, v.map(_._2))}

}

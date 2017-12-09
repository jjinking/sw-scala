package swcala.unit

import org.scalatest._
import swscala._


class Ch2Spec extends FlatSpec with Matchers {

  "Exercises 1" should "pass all tests" in {

    // Problem 1
    Exercises1.p1 shouldEqual Seq((0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (0,7), (0,8), (0,9), (1,0), (1,1), (1,2), (1,3), (1,4), (1,5), (1,6), (1,7), (1,8), (1,9), (2,0), (2,1), (2,2), (2,3), (2,4), (2,5), (2,6), (2,7), (2,8), (2,9), (3,0), (3,1), (3,2), (3,3), (3,4), (3,5), (3,6), (3,7), (3,8), (3,9), (4,0), (4,1), (4,2), (4,3), (4,4), (4,5), (4,6), (4,7), (4,8), (4,9), (5,0), (5,1), (5,2), (5,3), (5,4), (6,0), (6,1), (6,2), (7,0), (7,1), (7,2), (8,0), (8,1), (9,0), (9,1))

    // Problem 2
    Exercises1.p2(Seq("a", "b", "c"), Seq(false, true, true)) shouldEqual Seq("b", "c")

    // Problem 3
    Exercises1.p3(Seq(1,3,2,4)) shouldEqual Seq((1,true),(3,false),(2,true))

    // Problem 4
    Exercises1.p4(Seq(true, false), Seq(Set(1), Set(2))) shouldEqual Map(Set(1) -> true, Set(2) -> false)

    // Problem 5
    Exercises1.p5(Seq("a", "b", "c"), Seq(3, 1, 2)) shouldEqual Seq("b", "c", "a")
  }
}

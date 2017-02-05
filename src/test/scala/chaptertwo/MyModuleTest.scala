package chaptertwo

import org.scalatest.FunSuite
import MyModule._
import chapter3.datastructures.{List, _}
import chapter3.datastructures.List._

/**
  * Created by Kirill on 1/30/2017.
  */
class MyModuleTest extends FunSuite {
  test("Negative and positive values should produce posistive abs") {
    assert(abs(12) === 12)
    assert(abs(-9) === 9)
    assert(formatResult("absolute", -9, abs) === "The absolute of -9 is 9")
    assert(formatResult("factorial", 2, factorial) === "The factorial of 2 is 2")
  }

  test("isSorted detects all kinds of arrays") {

    assert(isSorted(Array(3.4, 5.6), (a: Double, b: Double) => a < b))
    assert(!isSorted(Array(56.9, 3.4, 5.6), (a: Double, b: Double) => a < b))
    assert(isSorted(Array(5.6), (a: Double, b: Double) => a < b))

    def stringCompare(a: String, b: String) = a < b

    assert(isSorted(Array("asd", "awe"), stringCompare))

    val lessThenString = new Function2[String, String, Boolean] {
      def apply(a: String, b: String) = a < b
    }

    assert(isSorted(Array("asd", "awe"), lessThenString))
  }

  test("curry") {
    def stringConcatCurry = curry((a: String, b: Int) => (a.toInt + b).toDouble)

    val result = stringConcatCurry("3")(2)
    assert(result === 5.0)
  }

  test("uncurry") {
    def convUncurry = uncurry((a: String) => (b: Int) => (a.toInt + b).toDouble + 2.0)

    val result = convUncurry("4", 5)
    assert(result === 11.0)
  }

  test("drop ,while, iniit") {
    import chapter3.datastructures.Cons
    import chapter3.datastructures.List._
    import chapter3.datastructures.Nil

    val dropped = drop(Cons("ab", Cons("cd", Cons("kj", Nil))), 2)
    assert(dropped == Cons("kj", Nil))

    val droppedWhile = dropWhile(Cons(1, Cons(2, Cons(3, Nil))))(x => x < 3)
    assert(droppedWhile == Cons(3, Nil))

    val inited = init(Cons("ab", Cons("cd", Cons("kj", Nil))))
    assert(inited == Cons("ab", Cons("cd", Nil)))
  }

  /*
    test("foldRight with Ni and Cons"){
      import chapter3.datastructures.Cons
      import chapter3.datastructures.List._
      import chapter3.datastructures.Nil

      val tested = foldRight(chapter3.datastructures.List(1,2,3),Nil:List[Int])(Cons(_,_))
    }
  */

  test("length") {
    val tested = length(List(1, 2, 3))
    assert(tested === 3)
    assert(length(List("as", "opo", "iioip", "iop")) === 4)
  }

  test("foldLeft") {
    val fl = foldLeft(List(1, 1, 1), 0)((a, b) => a + b)
    assert(fl === 3)
  }

  test("reverse") {
    assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  test("foldLeftByR") {
    val l = List(2, 3, 4)
    val normalL = foldLeft(l, 0)((a, b) => a - b)
    val normalR = foldRight(l, 0)((a, b) => a - b)
    assert(normalL == 3)
    assert(normalR == 3)
    assert(foldLeftByR(l, 0)((a, b) => a - b) == normalL)
  }

  test("concat") {
    val concated = concat(List(List(1, 2, 3), List(8, 9, 0)))
    assert(concated === List(1, 2, 3, 8, 9, 0))
  }
}

package chaptertwo

import org.scalatest.FunSuite
import MyModule._
import chapter3.datastructures.Cons

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
    import chapter3.datastructures.List._
    import chapter3.datastructures.Nil

    val dropped = drop(Cons("ab", Cons("cd", Cons("kj", Nil))), 2)
    assert(dropped == Cons("kj", Nil))

    val droppedWhile = dropWhile(Cons(1, Cons(2, Cons(3, Nil))))(x => x < 3)
    assert(droppedWhile == Cons(3, Nil))

    val inited = init(Cons("ab", Cons("cd", Cons("kj", Nil))))
    assert(inited == Cons("ab", Cons("cd", Nil)))
  }
}

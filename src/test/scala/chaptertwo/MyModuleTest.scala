package chaptertwo

import org.scalatest.FunSuite
import MyModule._

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

    def stringCompare(a: String,b: String) = a < b
    assert(isSorted(Array("asd", "awe"), stringCompare))

    val lessThenString = new Function2[String,String,Boolean]{
      def apply(a:String,b: String) = a < b
    }

    assert(isSorted(Array("asd", "awe"), lessThenString))
  }
}

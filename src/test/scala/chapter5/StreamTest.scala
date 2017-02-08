package chapter5

import org.scalatest.FunSuite
import chapter5.Stream._

/**
  * Created by Kirill on 2/8/2017.
  */
class StreamTest extends FunSuite {

  test("Stream functions") {
    val stream = Stream(23, 45, 65)
    assert(toList(stream) == List(23, 45, 65))
    assert(toList(take(stream, 2)) === List(23, 45))
    assert(toList(take(stream, 0)) === Nil)
    assert(toList(takeWhile(stream, (x: Int) => x < 50)) == List(23, 45))

    assert(stream.exist(_ == 45) == true)
    assert(stream.exist(_ == 5) == false)
    assert(stream.takeWhile((x: Int) => x < 50).toList == List(23, 45))
  }

}

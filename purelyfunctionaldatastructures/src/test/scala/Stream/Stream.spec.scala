package Stream

import org.scalatest._

class StreamQueueTest extends FunSpec with Matchers with MockFactor {
  describe("Stream") {
    it("Can store two values") {
      val optionOne: Some[String] = Some("first")
      lazy val optionTwo: Some[String] = Some("second")
      val testStream: Stream[String, String] = Stream(optionOne, optionTwo)
      assert(testStream.f == optionOne)
      assert(testStream.s() == optionTwo)
    }
  }
}

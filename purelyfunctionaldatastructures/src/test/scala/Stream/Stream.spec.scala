package Stream

import org.scalatest._

class StreamQueueTest extends FunSpec with Matchers {
  describe("Stream") {
    describe("base case") {
      it("I can create a base stream") {
        val testVal: Some[String] = Some("first")
        val testStream = Stream(testVal, None)
        assert(testStream.f == testVal)
        assert(testStream.s.isEmpty)
      }
    }
    describe("n + case") {
      it("Can store a nested recursive Stream") {
        val testVal = Some("first")
        val testStream = Stream(testVal, None)
        val testVal2 = Some("second")
        val testStream2 = Stream(testVal, Some(testStream))


        assert(testStream2.f == testVal2)
        assert(testStream2.s.get.f == testVal)
      }
    }
  }
}

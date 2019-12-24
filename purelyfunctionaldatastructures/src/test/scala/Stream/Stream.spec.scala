package Stream

import org.scalatest._

class StreamQueueTest extends FunSpec with Matchers {
  describe("Stream") {
    describe("base case") {
      it("Should create an empty Stream") {
        val testStream = Stream[String]()
        assert(testStream.f == None)
        assert(testStream.s == None)
      }
    }
    describe("n + case") {
      it("Can store a nested recursive Stream") {
        val testVal = Some("first")
        val testStream = Stream(testVal, None)
        val testVal2 = Some("second")
        val testStream2 = Stream(testVal2, Some(testStream))
        assert(testStream2.f == testVal2)
        assert(testStream2.s.get.f == testVal)
      }

      it("I can create a stream with only one element") {
        val testVal: Some[String] = Some("first")
        val testStream = Stream(testVal, None)
        assert(testStream.f == testVal)
        assert(testStream.s.isEmpty)
      }
    }
  }
}

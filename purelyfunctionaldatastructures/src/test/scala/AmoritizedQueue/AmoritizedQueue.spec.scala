package AmoritizedQueue

import org.scalatest._

class AmoritizedQueueTest extends FunSpec with Matchers {
  describe("AmoritizedQueue") {
    val testQueue = AmoritizedQueue[String]
    describe("head") {
      describe("Empty queue") {
        it("Should return the default value when the queue is empty") {
          val testQueue = AmoritizedQueue[String]
          assert(testQueue.head.isEmpty)
        }
      }
    }

    describe("tail") {
      describe("Empty queue"){
        it("Should return an empty queue") {
          val emptyQueue = testQueue.tail
          assert(emptyQueue.head.isEmpty)
        }
      }

      describe("Non empty queue") {
      }
    }

    describe("snoc") {
      it("Should return a queue after adding an item") {
        val testString = "testString"
        val queueWithOneItem = testQueue.snoc(testString)
        assert(queueWithOneItem.head.get == testString)
      }
    }
  }
}

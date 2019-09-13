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

    describe("snoc") {
      it("Should return a queue after adding an item") {
        val testString = "testString"
        val queueWithOneItem = testQueue.snoc(testString)
        assert(queueWithOneItem.head.get == testString)
      }

      it("Should return the correct queue after adding another item") {
        val testString = "testString"
        val secondTestString = "testString2"
        val queueWithOneItem = testQueue.snoc(testString)
        val queueWithTwoItems = queueWithOneItem.snoc(secondTestString)
        assert(queueWithTwoItems.head.get == testString)
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
        it("Should dequeue from the queue and the head should be the second item added") {
          val testValue = "value1"
          val testValue2 = "value2"
          val queueWithStuff = testQueue.snoc(testValue).snoc(testValue2)
          val tailedQueue = queueWithStuff.tail
          assert(tailedQueue.head.get == testValue2)
        }

        it("Should dequeue twice and return the correct head") {
          val testValue = "value1"
          val testValue2 = "value2"
          val testValue3 = "value3"
          val queueWithStuff = testQueue.snoc(testValue).snoc(testValue2).snoc(testValue3)
          val tailedQueue = queueWithStuff.tail.tail
          assert(tailedQueue.head.get == testValue3)
        }
      }
    }
  }
}

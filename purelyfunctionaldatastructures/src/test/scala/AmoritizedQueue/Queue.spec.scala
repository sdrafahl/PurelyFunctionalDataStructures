package Queues

import org.scalatest._

class QueueTest extends FunSpec with Matchers {

  describe("Queue") {
    queueTest(AmoritizedQueue[String], "Amoritized Queue")
    queueTest(RealTimeQueue[String], "Real Time Queue")
  }

  def queueTest(testQueue: Queue[String], testName: String) = {
    describe(testName) {
      describe("head") {
        describe("Empty queue") {
          it(s"Should return the default value when the queue is empty - ${testName}") {
            assert(testQueue.head.isEmpty)
          }
        }
      }

      describe("snoc") {
        it(s"Should return a queue after adding an item - ${testName}") {
          val testString = "testString"
          val queueWithOneItem = testQueue.snoc(testString)
          assert(queueWithOneItem.head.get == testString)
        }

        it(s"Should return the correct queue after adding another item - ${testName}") {
          val testString = "testString"
          val secondTestString = "testString2"
          val queueWithOneItem = testQueue.snoc(testString)
          val queueWithTwoItems = queueWithOneItem.snoc(secondTestString)
          assert(queueWithTwoItems.head.get == testString)
        }
      }

      describe("tail") {
        describe("Empty queue"){
          it(s"Should return an empty queue - ${testName}") {
            val emptyQueue = testQueue.tail
            assert(emptyQueue.head.isEmpty)
          }
        }

        describe("Non empty queue") {
          it(s"Should dequeue from the queue and the head should be the second item added - ${testName}") {
            val testValue = "value1"
            val testValue2 = "value2"
            val queueWithStuff = testQueue.snoc(testValue).snoc(testValue2)
            val tailedQueue = queueWithStuff.tail
            assert(tailedQueue.head.get == testValue2)
          }

          it(s"Should dequeue twice and return the correct head - ${testName}") {
            val testValue = "value1"
            val testValue2 = "value2"
            val testValue3 = "value3"
            val queueWithStuff = testQueue.snoc(testValue).snoc(testValue2).snoc(testValue3)
            val tailedQueue = queueWithStuff.tail.tail
            assert(tailedQueue.head.get == testValue3)
          }
        }
      }
      describe("isEmpty") {

        it(s"Should be true for empty queue - ${testName}") {
          assert(testQueue.isEmpty)
        }
        it(s"Should be false for non empty queue - ${testName}") {
          val nonEmptyQueue: Queue[String] = testQueue.snoc("testString")
          assert(!nonEmptyQueue.isEmpty)
        }
      }
    }
  }
}

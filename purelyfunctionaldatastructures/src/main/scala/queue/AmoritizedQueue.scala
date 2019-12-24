package Queues

import Stream._

abstract class AmoritizedQueue[A] extends Queue[A]

object AmoritizedQueue {
  implicit def apply[A]: AmoritizedQueue[A] = createQueue[A](None, None, 0, 0)

  private def createQueue[A](frontStream: Option[Stream[A]], rearStream: Option[Stream[A]], frontLength: Int, reverseLength: Int) : AmoritizedQueue[A] = new AmoritizedQueue[A] {

    def isEmpty = head match {
      case None => true
      case Some(_) => false
    }

    def head = frontStream match {
      case None => None
      case Some(stream) => stream.f  
    }

    def tail = frontStream match {
      case None => AmoritizedQueue[A]
      case Some(stream) =>  {
        val newFrontStream = stream.s
        lazy val newQueue = createQueue(newFrontStream, rearStream, frontLength - 1, reverseLength)
        check(newQueue, frontLength - 1, reverseLength, frStream = newFrontStream)
      }
    }

    def check(queue: => AmoritizedQueue[A], frontL: Int, rearL: Int, frStream: Option[Stream[A]] = frontStream, reStream: Option[Stream[A]] = rearStream): Queue[A] = if (frontL >= rearL) queue else rotate(frStream, reStream)

    def scons(item: A, second: Option[Stream[A]]) = Stream(Some(item), rearStream)

    def snoc(item: A): Queue[A] = {
      val newRearStream = scons(item, rearStream)
      lazy val newQueue = createQueue(frontStream, Some(newRearStream), frontLength, reverseLength + 1)
      check(newQueue, frontLength, reverseLength + 1, reStream = Some(newRearStream))
    }

    private[this] def appendToStream(st: Option[Stream[A]], secondSt: Option[Stream[A]]): Option[Stream[A]] = {
      st match {
        case None => secondSt
        case Some(stream) => {
          Some(Stream(stream.f, appendToStream(stream.s, secondSt)))
        }
      }
    }

    private[this] def rotate(fStream: Option[Stream[A]], rStream: Option[Stream[A]]): Queue[A] = {
      val reversedRearStream = reverseStream(inputStream = rStream)
      val newFrontStream = appendToStream(fStream, reversedRearStream)
      createQueue(newFrontStream, None, frontLength + reverseLength, 0)
    }

    private[this] def reverseStream(inputStream: Option[Stream[A]], result: Option[Stream[A]] = None): Option[Stream[A]] = {
      inputStream match {
        case Some(stream) => {
          val secondStream: Stream[A] = Stream(stream.f, result)
          reverseStream(stream.s, Some(secondStream))
        }
        case None => result
      }
    }
  }
}

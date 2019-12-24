package Queues

import Stream._

abstract class RealTimeQueue[A] extends Queue[A]

object RealTimeQueue {
  def apply[A]: RealTimeQueue[A] = createNewQueue[A]()

  def createNewQueue[A](fStream: Option[Stream[A]] = None, rStream: Option[Stream[A]] = None, schedule: Option[Stream[A]] = None):RealTimeQueue[A] = {
    new RealTimeQueue[A] {
      private def exec[A](firstStream: Option[Stream[A]] = None, rearStream: Option[Stream[A]] = None, sche: Option[Stream[A]]):RealTimeQueue[A] = {
        (firstStream, rearStream, sche) match {
          case (Some(_), _, Some(sched)) => createNewQueue(firstStream, rearStream, sched.s)
          case (None, Some(_), None) => createNewQueue(rearStream, None, None)
          case (Some(_), Some(_), None) => {
            val rotated = rotateStream[A](firstStream, rearStream, None)
            createNewQueue[A](rotated, None, rotated)
          }
        }
      }

      override def isEmpty: Boolean = fStream == None && rStream == None
      override def snoc(item: A): RealTimeQueue[A] = exec(fStream, Some(Stream(Some(item), rStream)), schedule)
      override def tail: RealTimeQueue[A] = {
        fStream match {
          case None => createNewQueue[A](None, None, None)
          case Some(frontStream) => exec[A](frontStream.s, rStream, schedule)
        }
      }
      def head: Option[A] = fStream match {
        case None => None
        case Some(fStream) => fStream.f
      }
    }
  }
  def rotateStream[A](str1: Option[Stream[A]], str2: Option[Stream[A]], aggregator: Option[Stream[A]]): Option[Stream[A]] = {
    (str1, str2, aggregator) match {
      case (None, None, _) => aggregator
      case (None, Some(stream2), _) => Some((Stream(stream2.f, aggregator)))
      case (Some(stream1), Some(stream2), _) => Some(Stream(stream1.f, rotateStream[A](stream1.s, stream2.s, Some(Stream(stream2.f, aggregator)))))
    }
  }
}

package AmoritizedQueue

import Stream._

abstract class AmoritizedQueue[A] {
  def head : Option[A]
  def tail : AmoritizedQueue[A]
  def snoc(item: A) : AmoritizedQueue[A] 
}

object AmoritizedQueue {
  implicit def apply[A]: AmoritizedQueue[A] = createQueue[A](None, None, 0, 0)

  private def createQueue[A](frontStream: Option[Stream[A]], rearStream: Option[Stream[A]], frontLength: Int, secondLength: Int) : AmoritizedQueue[A] = new AmoritizedQueue[A] {
    def head = frontStream match {
      case None => None
      case Some(stream) => stream.f  
    }

    def tail = frontStream match {
      case None => AmoritizedQueue[A]
    }

    def snoc(item: A): AmoritizedQueue[A] = {
      val newRearStream = Stream(Some(item), rearStream)
      val newQueue = createQueue(frontStream, Some(newRearStream), frontLength, secondLength + 1)
      if (frontLength == secondLength) newQueue else rotate(newRearStream)
    }

    lazy val frontLength: Int = frontLength

    lazy val reverseLength: Int = secondLength

    private[this] def appendToStream(st: Option[Stream[A]], secondSt: Option[Stream[A]]): Option[Stream[A]] = {
      st match {
        case None => secondSt
        case Some(stream) => {
          Some(Stream(stream.f, appendToStream(st.get.s, secondSt)))
        }
      }
    }

    private[this] def rotate(newRearStream: Stream[A]): AmoritizedQueue[A] = {
      val reversedRearStream = reverseStream(inputStream = Some(newRearStream)).get
      val newFrontStream = appendToStream(frontStream, Some(reversedRearStream))
      createQueue(newFrontStream, None, frontLength + secondLength, 0)
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

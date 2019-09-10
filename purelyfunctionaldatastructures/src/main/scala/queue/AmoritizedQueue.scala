package AmoritizedQueue

import Stream._

abstract class AmoritizedQueue[A] {
  def head : Option[A]
  def tail : AmoritizedQueue[A]
  def snoc(item: A) : AmoritizedQueue[A] 
}

object AmoritizedQueue {
  implicit def apply[A]: AmoritizedQueue[A] = createQueue[A](None, None, 0, 0)

  private def createQueue[A](frontStream: Option[Stream[A, Stream[_, _]]], rearStream: Option[Stream[A, Stream[_, _]]], frontLength: Int, secondLength: Int) : AmoritizedQueue[A] = new AmoritizedQueue[A] {
    def head = frontStream match {
      case None => None
      case Some(stream) => stream.f  
    }

    def tail = frontStream match {
      case None => AmoritizedQueue[A]
    }

    def snoc(item: A): AmoritizedQueue[A]  = frontLength == secondLength match {
      case true => rotate
      case false => createQueue(frontStream, Stream(Some(item), rearStream), frontLength, secondLength + 1)
    }

    lazy val frontLength: Int = frontLength

    lazy val reverseLength: Int = secondLength

    private[this] def appendToStream(st: Option[Stream[A, Option[Stream[_, _]]]], secondSt: Option[Stream[A, Option[Stream[_, _]]]]): Stream[A, Option[Stream[_, _]]] = {
      st match {
        case None => secondSt
        case Some(stream) => Stream(stream.f, appendToStream(stream.s)) 
      }
    }

    private[this] def rotate: AmoritizedQueue[A] = {
      val reversedRearStream = reverseStream(inputStream = rearStream).get
      val newFrontStream = appendToStream(frontStream, reversedRearStream)
      createQueue(newFrontStream, None, frontLength + secondLength, 0)
    }

    private[this] def reverseStream(inputStream: Option[Stream[Option[A], Option[Stream[_, _]]]], result: Option[Stream[Option[A], Option[Stream[_, _]]]] = None): Option[Stream[A, A]] = {
      inputStream match {
        case Some(stream) => reverseStream(stream.s, Stream(stream.f, result))
        case None => result
      }
    }
  }
}

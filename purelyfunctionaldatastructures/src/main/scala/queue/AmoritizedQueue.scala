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

    def snoc(item: A) = frontLength == secondLength || reverseLength == 0 match {
      case true =>
      case false => 
    }

    lazy val frontLength = frontLength

    lazy val reverseLength = secondLength

    private def rotate =

    private def reverseStream(inputStream: Stream[Option[A], Option[Stream[_, _]]], result: Stream[Option[A], Option[Stream[_, _]]]): Stream[A, A] = {
      inputStream.s match {
        case Some(_) => reverseStream(inputStream.s, Stream[inputStream.f, None])
        case None => Stream(inputStream.f, result)
      }
    }
  }
}

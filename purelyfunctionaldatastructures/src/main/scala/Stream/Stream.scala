package Stream

abstract class Stream[A, B] {
  val f : Option[A]
  val s : Option[B]
}

object Stream {
  implicit def apply[A, B](first:Option[A], second: => Option[B]) = new Stream[A, B] {
    val f = first
    lazy val s = second
  }
}

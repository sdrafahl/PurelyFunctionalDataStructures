package Stream

trait Stream[A] {
  type T = Stream[A]
  val f : Option[A]
  val s : Option[T]
}

object Stream {
  def apply[A](first:Option[A], second: => Option[Stream[A]]): Stream[A] = new Stream[A] {
    val f = first
    lazy val s = second
  }
}

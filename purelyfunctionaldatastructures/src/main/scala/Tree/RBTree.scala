package Tree

object Color extends Enumeration {
  type Color = Value
  val Red, Black = Value
}

trait Tree[A] {
  type T = Tree[A]
  val value : Option[A]
  val left : Option[T]
  val right : Option[T]
  val color : Color
  def member(item: A) : Boolean
  def insert(item: A) : Tree[A]
}

object Tree {}

package Tree

object Color extends Enumeration {
  type Color = Value
  val Red, Black = Value
}
import Color._

abstract class Tree[A] {
  type T = Tree[A]
  val value : Option[A]
  val left : Option[T]
  val right : Option[T]
  val color : Color
  def member(item: A) : Boolean
  def insert(item: A) : Tree[A]
}

case class RedBlackTree[A](value: Option[A] = None, left: Option[Tree[A]] = None, right: Option[Tree[A]] = None, color: Color = Black) extends Tree[A] {
  override def member(item: A) : Boolean = value match {
    case Some(valueOfNode) => item == valueOfNode
    case None => false
  }
  override def insert(item: A) : Tree[A] = ???
}


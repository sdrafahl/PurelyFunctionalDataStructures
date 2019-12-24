package Queues

abstract class Queue[A] {
  def snoc(item: A): Queue[A]
  def tail: Queue[A]
  def head: Option[A]
  def isEmpty: Boolean
}

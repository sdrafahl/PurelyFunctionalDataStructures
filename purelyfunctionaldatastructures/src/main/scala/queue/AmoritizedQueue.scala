package AmoritizedQueue

abstract class AmoritizedQueue[A] {
  def head : A
  def tail : AmoritizedQueue[A]
  def snoc : AmoritizedQueue[A] 
}

package List

case class Node(item: Ordered, child: Node)

abstract class FunctionalList {
  def append(value: Ordered): FunctionalList
}

object FunctionalList {
  def apply() = new FunctionalList
  implicit lazy val functionalList = FunctionalList() 
}

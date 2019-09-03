package List

case class Node(item: Int, child: Option[Node])

abstract class FunctionalList {
  val head : Option[Node]
  val tail : Option[Node]
  def append(value: Int): FunctionalList
}

object FunctionalList {
  def apply(givenHead: Option[Node] = None, givenTail: Option[Node] = None) = FunctionalList {
    val head = givenHead
    val tail = givenTail
    def append(value: Int): FunctionalList = {
      def appendNode(value : Int, node: Node): Node = node match {
        case Node(v, None) => Node(v, Node(value, None))
        case Node(v, c) => Node(v, appendNode(value, c))
      }
      def getTail(node: Node): Node = node match {
        case Node(_, None) => node
        case Node(_, c) => getTail(c)
      }
      head match {
        case None => {
          val newNode = Some(Node(value, None))
          FunctionalList(newNode, newNode)
        }
        case _ => {
          val newLinkedNodes = appendNode(valpue, head)
          val tail = getTail(newLinkedNodes)
          FunctionalList(head, newLinkedNodes)
        }
      }
    }
  }
  implicit lazy val functionalList = FunctionalList() 
}

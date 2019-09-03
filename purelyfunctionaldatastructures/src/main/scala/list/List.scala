package List

case class Node(item: Int, child: Option[Node])

abstract class FunctionalList {
  val head : Option[Node]
  val tail : Option[Node]
  def append(value: Int): FunctionalList
}

object FunctionalList {
  def apply(givenHead: Option[Node] = None, givenTail: Option[Node] = None): FunctionalList = new FunctionalList {
    val head = givenHead
    val tail = givenTail
    def append(value: Int): FunctionalList = {
      def appendNode(value : Int, node: Node): Node = node match {
        case Node(v, None) => Node(v, Some(Node(value, None)))
        case Node(v, Some(c)) => Node(v, Some(appendNode(value, c)))
      }
      def getTail(node: Node): Node = node match {
        case Node(_, None) => node
        case Node(_, Some(c)) => getTail(c)
      }
      head match {
        case None => {
          val newNode = Some(Node(value, None))
          FunctionalList(newNode, newNode)
        }
        case Some(n) => {
          val newLinkedNodes = appendNode(value, n)
          val tail = getTail(newLinkedNodes)
          FunctionalList(Some(newLinkedNodes), Some(tail))
        }
      }
    }
    override def toString: String = {
      def nodesToString(node: Node): String = {
        node match {
          case Node(v, None) => "" + v
          case Node(v, Some(c)) => v + "|" + nodesToString(c)
        }
      }
      head match {
        case None => "|"
        case Some(n) => nodesToString(n)
      }
    }
  }
  implicit lazy val functionalList = FunctionalList() 
}

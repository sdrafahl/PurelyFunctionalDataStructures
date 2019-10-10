package Tree

import java.lang.Comparable

object Color extends Enumeration {
  type Color = Value
  val Red, Black = Value
}
import Color._

abstract class Tree[A <: Comparable[A]] {
  type T = Tree[A]
  val value : A
  val left : Option[T]
  val right : Option[T]
  val color : Color
  def member(item: A) : Boolean
  def insert(item: A) : Tree[A]
}

case class RedBlackTree[A <: Comparable[A]](value: A, left: Option[RedBlackTree[A]] = None, right: Option[RedBlackTree[A]] = None, color: Color = Black) extends Tree[A] {
  override def member(item: A) : Boolean = {
    def mem(node: Option[RedBlackTree[A]]): Boolean = {
      node match {
        case Some(RedBlackTree(valueOfNode, _, Some(rightBranch), _)) if(item.compareTo(valueOfNode) > 0 ) => mem(Some(rightBranch))
        case Some(RedBlackTree(valueOfNode, Some(leftBranch), _, _)) if(item.compareTo(valueOfNode) < 0) => mem(Some(leftBranch))
        case Some(RedBlackTree(valueOfNode, _, _, _)) if(item.compareTo(valueOfNode) == 0) => true
        case _ => false
      }
    }
    mem(Some(this))
  }

  def balance(color: Color, left: Option[RedBlackTree[A]], right: Option[RedBlackTree[A]], value: A): Option[RedBlackTree[A]] = {
    (left, right) match {
      case (Some(RedBlackTree(valueOfLeftGrandChild, Some(RedBlackTree(valueOfGreatGrandChild: A, ggcleft, ggcright, Red)), gcright, Red)), _) if color == Black => {
          Some(RedBlackTree[A](valueOfLeftGrandChild, Some(RedBlackTree[A](valueOfGreatGrandChild, ggcleft, ggcright, Black)), Some(RedBlackTree[A](value, gcright, right, Black)), Red))
      }
      case (Some(RedBlackTree(valueOfLeftGrandChild, gcleft, Some(RedBlackTree(valueOfGreatGrandChild: A, ggcleft, ggcright, Red)), Red)), _) if color == Black => {
          Some(RedBlackTree[A](valueOfGreatGrandChild, Some(RedBlackTree[A](valueOfLeftGrandChild, gcleft, ggcleft, Black)), Some(RedBlackTree[A](value, ggcright, right, Black)), Red))
      }
      case (_ ,Some(RedBlackTree(valueOfRightGrandChild, Some(RedBlackTree(valueOfGreatGrandChild: A, ggcleft, ggcright, Red)), gcright, Red))) if color == Black => {
          Some(RedBlackTree[A](valueOfGreatGrandChild, Some(RedBlackTree[A](value, left, ggcleft, Black)), Some(RedBlackTree[A](valueOfRightGrandChild, ggcright, gcright, Black)), Red))
      }
      case (_ ,Some(RedBlackTree(valueOfRightGrandChild, gcleft, Some(RedBlackTree(valueOfGreatGrandChild: A, ggcleft, ggcright, Red)), Red))) if color == Black => {
          Some(RedBlackTree[A](valueOfRightGrandChild, Some(RedBlackTree[A](value, left, gcleft, Black)), Some(RedBlackTree[A](valueOfGreatGrandChild, ggcleft, ggcright, Black)), Red))
      }
      case _ => {
        Some(RedBlackTree[A](value, left, right, color)) 
      }
    }
  }

  override def insert(item: A) : Tree[A] = {
        def ins(node: Option[RedBlackTree[A]]) : Option[RedBlackTree[A]] = {
          node match {
            case None => {
              Some(Tree.RedBlackTree[A](item, color = Red))
            }
            case Some(RedBlackTree(rootValue, _, _, _)) => {
              if(item.compareTo(rootValue) <= 0) {
                balance(node.get.color, ins(node.get.left), node.get.right, rootValue)
              } else {
                balance(node.get.color, node.get.left, ins(node.get.right), rootValue)
              }
            }
          }
        }
    ins(Some(this)).get
  }
}


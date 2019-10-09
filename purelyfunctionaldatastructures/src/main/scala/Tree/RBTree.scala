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

case class RedBlackTree[A <: Comparable[A]](value: A, left: Option[RedBlackTree[A]] = None, right: Option[RedBlackTree[A]] = None, color: Color =Black) extends Tree[A] {
  override def member(item: A) : Boolean = value match {
    case Some(valueOfNode) => item == valueOfNode
    case None => false
  }

  def balance(color: Color, left: Option[RedBlackTree[A]], right: Option[RedBlackTree[A]], value: Option[A]): Option[RedBlackTree[A]] = {
    println("matchikng the values")
    println(left)
    println(right)
    println(color)
    println(value)
    left match {
      case Some(RedBlackTree(Some(valueOfTree), Some(RedBlackTree(valueOfLeftGrandChild, Some(RedBlackTree(Some(valueOfGreatGrandChild: A), ggcleft, ggcright, Red)), gcright, Red)), rightChild: Option[RedBlackTree[A]], Black)) => {
        println("has reached the case")
        Some(RedBlackTree[A](Some(valueOfLeftGrandChild), Some(RedBlackTree[A](valueOfGreatGrandChild, ggcleft, ggcright, Black)), Some(RedBlackTree[A](Some(valueOfTree), gcright, rightChild, Black)), Red))
      }
      case _ => Some(RedBlackTree[A](value, left, right, color)) 
    }
  }
//Some(RedBlackTree[A](Some(valueOfLeftGrandChild), Some(RedBlackTree[A](valueOfGreatGrandChild, ggcleft, ggcright, Black)), Some(RedBlackTree[A](Some(valueOfTree), gcright, rightChild, Black)), Red))
  override def insert(item: A) : Tree[A] = {
    value match {
      case None => {
        val someItem = Some(item)
        Tree.RedBlackTree[A](value = someItem)
      }
      case Some(_) => {
        def ins(node: Option[RedBlackTree[A]]) : Option[RedBlackTree[A]] = {
          node match {
            case None => {
              Some(Tree.RedBlackTree[A](item, color = Red))
            }
            case Some(RedBlackTree(rootValue, _, _, _)) => {
              if(item.compareTo(rootValue.get) <= 0) {
                println(balance(color, ins(node.get.left), right, rootValue))
                println("right here")
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
  }
}


package Tree

import org.scalatest._
import Color._
import scala.collection.immutable.RedBlackTree

class RedBlackTreeTest extends FunSpec with Matchers {
  describe("Tree") {
    val testString = "testString"
    val emptyTestTree = Tree.RedBlackTree[String]()
    val emptyTestTreeTwo = Tree.RedBlackTree[String]() 
    val testTree = Tree.RedBlackTree[String](value = Some(testString))

    val treeOne = Tree.RedBlackTree[String](Some(testString), None, None)
    val nDepthTree = Tree.RedBlackTree[String](Some(testString))
    it("has the correct default values") {
          emptyTestTree should have (
            'value (None),
            'left (None),
            'right (None),
            'color (Black)
          )
    }
    describe("insert") {
      describe("Base Case") {
        it("can work on a single empty node") {
          val treeWithOneValue = emptyTestTree.insert(testString)
          assert(treeWithOneValue.value == Some(testString))
        }
      }
      describe("n+1 case") {
        describe("can re-balance the tree after adding multiple nodes") {
          it("can reblance tree rotation case 1") {
            val tree = emptyTestTreeTwo.insert("z").insert("y").insert("x")
            println("hello world")
            println(tree)
            assert(tree.value.get == "z")
            assert(tree.left.get.value.get == "y")
            assert(tree.right.get.valuep.get == "x") 
          }
        }
      }
    }
    describe("member") {
      describe("Base Case") {
        it("can work on a single empty node") {
          assert(!emptyTestTree.member(testString))
        }
        it("can work on a single node") {
          assert(testTree.member(testString))
        }
      }
      describe("Tree with n depth") {

      }
    }
  }
}

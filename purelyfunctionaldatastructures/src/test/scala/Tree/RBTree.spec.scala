package Tree

import Color._
import org.scalatest._
import scala.collection.immutable.RedBlackTree

class RedBlackTreeTest extends FunSpec with Matchers {
  describe("Tree") {
    val testString = "testString"
    val testString2 = "testString2"
    val testTree = Tree.RedBlackTree[String](testString)
    val testTreeTwo = Tree.RedBlackTree[String]("z") 
    it("has the correct default values") {
          testTree should have (
            'value (testString),
            'left (None),
            'right (None),
            'color (Black)
          )
    }
    describe("insert") {
      describe("Base Case") {
        it("can work on a single empty node") {
          val baseTree = testTree.insert(testString)
          assert(baseTree.left.get.value == testString)
        }
      }
      describe("n+1 case") {
        describe("can re-balance the tree after adding multiple nodes") {
          it("can reblance tree rotation case 1") {
            val tree = testTreeTwo.insert("y").insert("x")
            assert(tree.value == "y")
            assert(tree.right.get.value == "z")
            assert(tree.left.get.value == "x") 
          }

          it("can reblance tree rotation case 2") {
            val testTreeThree = Tree.RedBlackTree[String]("z") 
            val tree = testTreeTwo.insert("a").insert("m")
            assert(tree.value == "m")
            assert(tree.left.get.value == "a")
            assert(tree.right.get.value == "z")
          }
        }
      }
    }
    describe("member") {
      describe("Base Case") {
        it("can work on a single empty node") {
          assert(!testTree.member("not the test string"))
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

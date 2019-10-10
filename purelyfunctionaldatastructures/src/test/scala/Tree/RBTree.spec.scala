package Tree

import Color._
import org.scalatest._
import scala.collection.immutable.RedBlackTree

class RedBlackTreeTest extends FunSpec with Matchers {
  describe("Tree") {
    val testString = "testString"
    val testTree = Tree.RedBlackTree[String](testString)
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
            val tree = Tree.RedBlackTree[String]("z").insert("y").insert("x")
            assert(tree.value == "y")
            assert(tree.right.get.value == "z")
            assert(tree.left.get.value == "x") 
          }

          it("can reblance tree rotation case 2") {
            val tree = Tree.RedBlackTree[String]("z").insert("a").insert("m") 
            assert(tree.value == "m")
            assert(tree.left.get.value == "a")
            assert(tree.right.get.value == "z")
          }

          it("can reblance tree roation case 3") {
            val tree = Tree.RedBlackTree[String]("a").insert("m").insert("c")
            assert(tree.value == "c")
            assert(tree.left.get.value == "a")
            assert(tree.right.get.value == "m")
          }

          it("can reblance tree roation case 4") {
            val tree = Tree.RedBlackTree[String]("a").insert("b").insert("c")
            assert(tree.value == "b")
            assert(tree.left.get.value == "a")
            assert(tree.right.get.value == "c")
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
        it("can work with a tree with multiple paths deep") {
          val tree = Tree.RedBlackTree[String]("a").insert("b").insert("c")
          assert(tree.member("a"))
          assert(tree.member("b"))
          assert(tree.member("c"))
        }
      }
    }
  }
}

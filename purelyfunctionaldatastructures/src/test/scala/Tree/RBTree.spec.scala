package Tree

import org.scalatest._
import Color._
import scala.collection.immutable.RedBlackTree

class RedBlackTreeTest extends FunSpec with Matchers {
  describe("Tree") {
    val testString = "testString"
    val emptyTestTree = RedBlackTree[String]()
    val testTree = RedBlackTree[String](value = Some(testString))

    val treeOne = RedBlackTree[String](Some(testString), None, None)
    val nDepthTree = RedBlackTree[String](Some(testString))
    it("has the correct default values") {
          emptyTestTree should have (
            'value (None),
            'left (None),
            'right (None),
            'color (Black)
          )
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

    describe("insert") {
      describe("Base Case") {
        it("can work on a single empty node") {
          //assert(false)
        }
      }
    }
  }
}
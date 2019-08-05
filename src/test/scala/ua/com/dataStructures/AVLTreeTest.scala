package ua.com.dataStructures

import org.scalatest.FunSuite
import ua.com.dataStructures.trees.{AVLTree, BST}

class AVLTreeTest extends FunSuite {

  test("can create tree") {
    val avlTree = AVLTree(1, 4, 23, 67, 11, 109)

    val avlHeight = avlTree.height
    val newTree = avlTree.insert(2)
    val newHeight = newTree.height

    assert(newHeight == 4)
    assert(avlHeight == 3)
  }

  test("can remove node from tree") {
    val avlTree = AVLTree(111, 4, 89, 67, 2, 55, 66)

    val newTree = avlTree.remove(4)
    val height = newTree.height

    assert(height == 3)
  }
}

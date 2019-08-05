package ua.com.dataStructures

import org.scalatest.FunSuite
import ua.com.dataStructures.linear._

class LinkedListTest extends FunSuite {

  test("can create LinkedList from sequence of elements") {
    val list = LinkedList(1, 2, 3, 4)
    assume(list.isInstanceOf[LinkedList])

    assert(list.size == 4)
  }

  test("can add new element to list") {
    val original = LinkedList(1, 2, 3, 4)
    val newList = original.::(5)

    assert(newList.size == 5)
  }

  test("can map list correctly") {
    val original = LinkedList(2, 3, 4)

    original.map(x => x * x) === LinkedList(4, 9, 16)
  }

  test("can reverse a list") {
    val original = LinkedList(1, 2, 3, 4, 5)

    original.reverse() === LinkedList(5, 4, 3, 2, 1)
  }

  test("can filter a list") {
    val original = LinkedList(1, 2, 3, 4, 5)

    original.filter(x => (x % 2) == 0) === LinkedList(2, 4)
  }

  test("can build lists with cons") {
    val list = 1 :: 2 :: 3 :: 4 :: Empty

    assert(list == LinkedList(1, 2, 3, 4))
  }

  test("can appending two lists") {
    val first = LinkedList(1, 2, 3, 4)
    val second = LinkedList(10, 11, 12, 13)

    assert(first.:::(second) === LinkedList(10, 11, 12, 13, 1, 2, 3, 4))
  }

}

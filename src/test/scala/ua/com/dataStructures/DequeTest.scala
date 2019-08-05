package ua.com.dataStructures

import org.scalatest.FunSuite
import ua.com.dataStructures.queues.Deque

class DequeTest extends FunSuite {

  test("addFirst can insert the specified element at the front of this deque") {
    val deque = Deque()

    val newDeque = deque.pushFront(11)

    assert(newDeque.popFront._1 == 11)
  }

  test("addLast can insert the specified element at the end of this deque") {
    val deque = Deque(12, 3, 40, 0, 11, 13)

    val newDeque = deque.pushLast(11)

    assert(newDeque.popLast._1 == 11)
    assert(newDeque.popFront._1 == 12)
  }
}

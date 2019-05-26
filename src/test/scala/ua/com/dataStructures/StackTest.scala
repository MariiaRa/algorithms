package ua.com.dataStructures

import org.scalatest.FunSuite
import ua.com.Data._
import ua.com.dataStructures.elementary.Stack

class StackTest extends FunSuite {
  test("push can add new item to the stack") {

    val stack = Stack(ints)
    val newStack = stack.push(111)

    assert(newStack.size == stack.size + 1)
  }

  test("pop can remove one item from the stack") {

    val stack = Stack(chars)
    val (popped, newStack) = stack.pop

    assert(popped.contains('s'))
    assert(newStack.size == stack.size - 1)
  }
}

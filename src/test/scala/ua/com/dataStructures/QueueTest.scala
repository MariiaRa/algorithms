package ua.com.dataStructures

import org.scalatest.FunSuite
import ua.com.dataStructures.elementary.Queue

class QueueTest extends FunSuite {

  test("enqueue can add new item to the queue") {
    val queue = Queue()

    val newQueue = queue.enqueue(12)

    assert(newQueue.head == 12)
  }

  test("dequeue can remove one item from the queue") {
    val queue = Queue(13, 6, 8, 9)

    val (elem, newQueue) = queue.dequeue

    assert(elem == 13)
    assert(newQueue.head == 6)
  }
}

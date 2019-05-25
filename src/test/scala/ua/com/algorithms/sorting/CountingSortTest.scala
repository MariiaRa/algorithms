package ua.com.algorithms.sorting

import org.scalatest.FunSuite
import ua.com.Data._
import ua.com.algorithms.sorting.internal.CountingSort

class CountingSortTest extends FunSuite {

  test("Counting sorting can sort list of integers") {
    val expected = List(2, 4, 11, 13, 33, 56, 57, 88, 90)

    val sorted = CountingSort.sort(ints, 0, 100)
    assert(sorted == expected)
    assert(sorted.head == 2)
    assert(sorted.last == 90)
  }
}
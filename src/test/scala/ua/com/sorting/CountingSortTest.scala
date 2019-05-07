package ua.com.sorting

import org.scalatest.FunSuite
import Data._

class CountingSortTest extends FunSuite {

  test("Counting sorting can sort list of integers") {
    val expected = List(2, 4, 11, 13, 33, 56, 57, 88, 90)

    val sorted = CountingSort.sort(ints, 0, 100)
    assert(sorted == expected)
    assert(sorted.head == 2)
    assert(sorted.last == 90)
  }
}
package ua.com.algorithms.sorting

import org.scalatest.FunSuite
import ua.com.algorithms.sorting.internal.BucketSort

class BucketSortTest extends FunSuite {
  test("Counting sorting can sort list of integers") {
    val elements = Array(3, 9, 8, 13, 2, 11, 5, 4, 12)

    val expected = List(2, 3, 4, 5, 8, 9, 11, 12, 13)

    val sorted = BucketSort.sort(elements, 5)
    assert(sorted == expected)
    assert(sorted.head == 2)
    assert(sorted.last == 13)
  }
}

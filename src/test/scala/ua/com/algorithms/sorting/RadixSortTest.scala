package ua.com.algorithms.sorting

import org.scalatest.FunSuite
import ua.com.algorithms.sorting.internal.RadixSort

class RadixSortTest extends FunSuite {

  test("Radix sorting can sort array of integers") {
    val input = Array(17, 45, 75, 90, 80, 24, 2, 66)
    val expected = Array(2, 17, 24, 45, 66, 75, 80, 90)

    val sorted = RadixSort.sort(input, 2, 10)

    assert(sorted.head == expected.head)
    assert(sorted.last == expected.last)
    sorted.zip(expected).map{ case (sortedElem, expectedElem) => assert(sortedElem == expectedElem)}
  }

}

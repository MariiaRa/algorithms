package ua.com.algorithms.sorting

import org.scalatest.FunSuite
import ua.com.algorithms.sorting.internal.ShellSort

class ShellSortTest extends FunSuite {

  val ints = Array(14, 56, 99, 66, 10, 2, 57, 88, 33, 49)
  val chars = Array('s', 'v', 'a', 'b', 'd')
  val doubles = Array(1.56, 45.03, 99.99, 0.33, 11.13, 5.005, 5.006, 23.05)

  test("Shell sorting can sort list of integers") {
    val expected = Array(2, 10, 14, 33, 49, 56, 57, 66, 88, 99)

    ShellSort.sortOne(ints)

    assert(ints.head == expected.head)
    assert(ints.last == expected.last)
    ints.zip(expected).map{ case (sortedElem, expectedElem) => assert(sortedElem == expectedElem)}
  }

  test("Shell sorting can sort list of characters") {
    val expected = Array('a', 'b', 'd', 's', 'v')

    ShellSort.sortOne(chars)

    assert(chars.head == expected.head)
    assert(chars.last == expected.last)
    chars.zip(expected).map{ case (sortedElem, expectedElem) => assert(sortedElem == expectedElem)}
  }

  test("Shell sorting can sort list of doubles") {
    val expected = Array(0.33, 1.56, 5.005, 5.006, 11.13, 23.05, 45.03, 99.99)

    ShellSort.sortOne(doubles)

    assert(doubles.head == expected.head)
    assert(doubles.last == expected.last)
    doubles.zip(expected).map{ case (sortedElem, expectedElem) => assert(sortedElem == expectedElem)}
  }
}

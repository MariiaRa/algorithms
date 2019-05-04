package ua.com.sorting

import org.scalatest.FunSuite
import Data._

class QuickSortTest extends FunSuite {

  test("Quick sorting can sort list of integers") {
    val expected = List(2, 4, 11, 13, 33, 56, 57, 88, 90)

    val sorted = QuickSort.sort(ints)

    assert(sorted == expected)
    assert(sorted.head == 2)
    assert(sorted.last == 90)
  }

  test("Quick sorting can sort list of characters") {
    val expected = List('a', 'b', 'd', 's', 'v')

    val sorted = QuickSort.sort(chars)

    assert(sorted == expected)
    assert(sorted.head == 'a')
    assert(sorted.last == 'v')
  }

  test("Quick sorting can sort list of doubles") {
    val expected = List(0.33, 1.56, 5.005, 5.006, 11.13, 23.05, 45.03, 99.99)

    val sorted = QuickSort.sort(doubles)

    assert(sorted == expected)
    assert(sorted.head == 0.33)
    assert(sorted.last == 99.99)
  }

}

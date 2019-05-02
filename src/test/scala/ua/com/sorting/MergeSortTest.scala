package ua.com.sorting

import org.scalatest.FunSuite

class MergeSortTest extends FunSuite {

  val ints = List(4, 56, 90, 13, 11, 2, 57, 88, 33)
  val chars = List('s', 'v', 'a', 'b', 'd')
  val doubles = List(1.56, 45.03, 99.99, 0.33, 11.13, 5.005, 5.006, 23.05)

  test("Insertion sorting can sort list of integers") {
    val expected = List(2, 4, 11, 13, 33, 56, 57, 88, 90)

    val sorted = MergeSort.sort(ints)

    assert(sorted == expected)
    assert(sorted.head == 2)
    assert(sorted.last == 90)
  }

  test("Insertion sorting can sort list of characters") {
    val expected = List('a', 'b', 'd', 's', 'v')

    val sorted = MergeSort.sort(chars)

    assert(sorted == expected)
    assert(sorted.head == 'a')
    assert(sorted.last == 'v')
  }

  test("Insertion sorting can sort list of doubles") {
    val expected = List(0.33, 1.56, 5.005, 5.006, 11.13, 23.05, 45.03, 99.99)

    val sorted = MergeSort.sort(doubles)

    assert(sorted == expected)
    assert(sorted.head == 0.33)
    assert(sorted.last == 99.99)
  }

}

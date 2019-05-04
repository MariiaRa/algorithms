package ua.com.sorting

import org.scalatest.FunSuite

class BubbleSortTest extends FunSuite {

  val ints = List(4, 56, 90, 13, 11, 2, 57, 88, 33)
  val chars = List('s', 'v', 'a', 'b', 'd')
  val doubles = List(1.56, 45.03, 99.99, 0.33, 11.13, 5.005, 5.006, 23.05)
  val strings = List("kappa", "gamma", "epsilon", "theta", "beta", "lambda", "alpha")

  test("Bubble sorting can sort list of integers") {
    val expected = List(2, 4, 11, 13, 33, 56, 57, 88, 90)

    val sorted = BubbleSort.sort(ints)

    assert(sorted == expected)
    assert(sorted.head == 2)
    assert(sorted.last == 90)
  }

  test("Bubble sorting can sort list of strings") {
    val expected = List("alpha", "beta", "epsilon", "gamma", "kappa", "lambda", "theta")

    val sorted = BubbleSort.sort(strings)

    assert(sorted == expected)
    assert(sorted.head == "alpha")
    assert(sorted.last == "theta")
    assert(sorted.length == 7)
  }

  test("Bubble sorting can sort list of characters") {
    val expected = List('a', 'b', 'd', 's', 'v')

    val sorted = BubbleSort.sort(chars)

    assert(sorted == expected)
    assert(sorted.head == 'a')
    assert(sorted.last == 'v')
  }

  test("Bubble sorting can sort list of doubles") {
    val expected = List(0.33, 1.56, 5.005, 5.006, 11.13, 23.05, 45.03, 99.99)

    val sorted = BubbleSort.sort(doubles)

    assert(sorted == expected)
    assert(sorted.head == 0.33)
    assert(sorted.last == 99.99)
  }
}

package ua.com.algorithms.sorting

import org.scalatest.FunSuite
import ua.com.Data._
import ua.com.algorithms.sorting.internal.HeapSort

import scala.collection.mutable
class HeapSortTest extends FunSuite {

  test("Heap sorting can sort list of integers") {
    val expected = Array(2, 10, 14, 33, 49, 56, 57, 66, 88, 99)

    val sorted = HeapSort.sort(array)

    sorted.zip(expected).map{ case (sortedElem, expectedElem) => assert(sortedElem == expectedElem)}
    assert(sorted.head == 2)
    assert(sorted.last == 99)
  }

  test("Heap sorting can sort list of strings") {
    val expected = Array("alpha", "beta", "epsilon", "gamma", "kappa", "lambda", "theta")

    val sorted = HeapSort.sort(strings.toArray)

    sorted.zip(expected).map{ case (sortedElem, expectedElem) => assert(sortedElem == expectedElem)}
    assert(sorted.head == "alpha")
    assert(sorted.last == "theta")
    assert(sorted.length == 7)
    }

  test("Heap sorting can sort list of characters") {
    val expected = Array('a', 'b', 'd', 's', 'v')

    val sorted = HeapSort.sort(chars.toArray)

    sorted.zip(expected).map{ case (sortedElem, expectedElem) => assert(sortedElem == expectedElem)}
    assert(sorted.head == 'a')
    assert(sorted.last == 'v')
  }

  test("Heap sorting can sort list of doubles") {
    val expected = Array(0.33, 1.56, 5.005, 5.006, 11.13, 23.05, 45.03, 99.99)

    val sorted = HeapSort.sort(doubles.toArray)

    sorted.zip(expected).map{ case (sortedElem, expectedElem) => assert(sortedElem == expectedElem)}
    assert(sorted.head == 0.33)
    assert(sorted.last == 99.99)
  }
}

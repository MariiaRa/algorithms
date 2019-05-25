package ua.com.algorithms.sorting

import org.scalatest.FunSuite
import ua.com.Data._
import ua.com.algorithms.sorting.internal.MergeSort

class MergeSortTest extends FunSuite {

  def buildStrem(take: Int): Stream[Int] =
    if (take == 0) Stream.Empty
    else Stream.cons(util.Random.nextInt(666), buildStrem(take - 1))

  test("Merge sorting can sort list of integers") {
    val expected = List(2, 4, 11, 13, 33, 56, 57, 88, 90)

    val sorted = MergeSort.sort(ints)
    val stream = buildStrem(10)
    val sortedStream = MergeSort.sortTwo(stream)

    assert(sorted == expected)
    assert(sorted.head == 2)
    assert(sorted.last == 90)
    assert(sortedStream.length == 10)
    assert(sortedStream.head < sortedStream.last)
    sortedStream.sliding(2).foreach{ pair => assert(pair.head < pair.last)}
  }

  test("Merge sorting can sort list of characters") {
    val expected = List('a', 'b', 'd', 's', 'v')

    val sorted = MergeSort.sort(chars)

    assert(sorted == expected)
    assert(sorted.head == 'a')
    assert(sorted.last == 'v')
  }

  test("Merge sorting can sort list of doubles") {
    val expected = List(0.33, 1.56, 5.005, 5.006, 11.13, 23.05, 45.03, 99.99)

    val sorted = MergeSort.sort(doubles)

    assert(sorted == expected)
    assert(sorted.head == 0.33)
    assert(sorted.last == 99.99)
  }

}

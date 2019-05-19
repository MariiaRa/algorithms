package ua.com.searching

import org.scalatest.FunSuite
import ua.com.Data.{chars, doubles, ints, strings}

class BinarySearchTest extends FunSuite {

  test("Binary search can find an integer in a list of integers") {
    val expectedIndex = 2
    val actualIndex = BinarySearch.search(ints.sorted, 11)

    assert(actualIndex == expectedIndex)
  }

  test("Binary search can find a string in a list of strings") {
    val expectedIndex = 3
    val actualIndex = BinarySearch.search(strings.sorted, "gamma")

    assert(actualIndex == expectedIndex)
  }

  test("Binary search can find a char in a list of characters") {
    val expectedIndex = 1
    val actualIndex = BinarySearch.search(chars.sorted, 'b')

    assert(actualIndex == expectedIndex)
  }

  test("Binary search can find a double in a list of doubles") {
    val expectedIndex = 1
    val actualIndex = BinarySearch.search(doubles.sorted, 1.56)

    assert(actualIndex == expectedIndex)
  }

  test("Binary search can't find an integer which is not present in a list of integers and will return -1") {
    val expectedIndex = -1
    val actualIndex = BinarySearch.search(ints.sorted, 100)

    assert(actualIndex == expectedIndex)
  }
}

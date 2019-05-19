package ua.com.searching

import org.scalatest.FunSuite
import ua.com.Data._
class LinearSearchTest extends FunSuite {

  test("Linear search can find an integer in a list of integers") {
    val expectedIndex = 4

    val actualIndex = LinearSearch.search(ints, 11)

    assert(actualIndex == expectedIndex)
  }

  test("Linear search can find a string in a list of strings") {
    val expectedIndex = 1

    val actualIndex = LinearSearch.search(strings, "gamma")

    assert(actualIndex == expectedIndex)
  }

  test("Linear search can find a char in a list of characters") {
    val expectedIndex = 3

    val actualIndex = LinearSearch.search(chars, 'b')

    assert(actualIndex == expectedIndex)
  }

  test("Linear search can find a double in a list of doubles") {
    val expectedIndex = 0

    val actualIndex = LinearSearch.search(doubles, 1.56)

    assert(actualIndex == expectedIndex)
  }

  test("Linear search can't find an integer which is not present in a list of integers and will return -1") {
    val expectedIndex = -1

    val actualIndex = LinearSearch.search(ints, 1130)

    assert(actualIndex == expectedIndex)
  }

}

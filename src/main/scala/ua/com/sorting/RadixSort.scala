package ua.com.sorting

object RadixSort {

  def sort(elements: Array[Int], numberOfDigits: Int, base: Int): Array[Int] = {
    for (digit <- 1 to numberOfDigits) {
      countSort(elements, Math.pow(base, digit).toInt, base)
    }
    elements
  }

  private def countSort(elements: Array[Int], digit: Int, base: Int) = {

    val countArray = elements.foldLeft(Array.fill(base)(0)) { (arr, integer) =>
      arr((integer/digit)%base) += 1
      arr
    }

    (1 until 10).foldLeft(countArray){ (arr, index) =>
      countArray(index) += countArray(index-1)
      countArray
    }

    val sortedByDigit = elements.reverse.foldLeft(Array.fill(elements.length)(0)) { (arr, integer) =>
      countArray((integer/digit)%base)  -= 1
      arr(countArray((integer/digit)%base)) = integer
      arr
    }

    sortedByDigit.copyToArray(elements, 0, elements.length)
    elements
  }
}

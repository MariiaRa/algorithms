package ua.com.algorithms.searching

import scala.annotation.tailrec

object BinarySearch {

  def search[T](elements: List[T], value: T)(implicit ord: Ordering[T]): Int = {

    @tailrec
    def recursivelySearch(start: Int, end: Int): Option[Int] = (start + end) / 2 match {
      case _ if end < start => None
      case middle if (ord.compare(value, elements(middle)) == 0) => Some(middle)
      case middle if (ord.compare(value, elements(middle)) < 0)  => recursivelySearch(start, middle - 1)
      case middle if (ord.compare(value, elements(middle)) > 0)  => recursivelySearch(middle + 1, end)
    }

    recursivelySearch(0, elements.length-1).getOrElse(-1)
  }
}

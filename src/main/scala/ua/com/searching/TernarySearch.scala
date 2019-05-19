package ua.com.searching

import scala.annotation.tailrec

object TernarySearch {

  def search[T](elements: List[T], value: T)(implicit ord: Ordering[T]): Int = {

    @tailrec
    def recursivelySearch(start: Int, end: Int): Option[Int] = {

      val middle1 = start + (end - start) / 3
      val middle2 = end - (end - start) / 3

      (middle1, middle2) match {
        case _ if end < start => None
        case (middle, _) if (ord.compare(value, elements(middle)) == 0) => Some(middle)
        case (_, middle) if (ord.compare(value, elements(middle)) == 0) => Some(middle)
        case (middle, _) if (ord.compare(value, elements(middle)) < 0) => recursivelySearch(start, middle - 1)
        case (_, middle) if (ord.compare(value, elements(middle)) > 0) => recursivelySearch(middle + 1, end)
        case (middle1, middle2) => recursivelySearch(middle1 + 1, middle2 - 1)
      }
    }
    recursivelySearch(0, elements.length - 1).getOrElse(-1)
  }
}
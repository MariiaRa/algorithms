package ua.com.algorithms.searching

import scala.annotation.tailrec

object InterpolationSearch {

  def search(elements: List[Int], value: Int): Int = {

    @tailrec
    def recursivelySearch(start: Int, end: Int): Option[Int] = {

      if (start <= end && value >= elements(start) && value <= elements(end)) {
        if (start == end) {
          if (elements(start) == value) start else -1
        }

        val position = start + (((end-start) / (elements(end) - elements(start)))*(value - elements(start)))
        position.toInt match {
          case _ if end < start => None
          case middle if elements(middle) == value => Some(middle)
          case middle if value < elements(middle) => recursivelySearch(start, middle - 1)
          case middle if value > elements(middle) => recursivelySearch(middle + 1, end)
        }
      } else {
        None
      }
    }
    recursivelySearch(0, elements.length - 1).getOrElse(-1)
  }
}

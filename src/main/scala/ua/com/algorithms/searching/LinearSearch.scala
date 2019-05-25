package ua.com.algorithms.searching

import scala.annotation.tailrec

object LinearSearch {

  def search[T](elements: List[T], value: T)(implicit ord: Ordering[T]): Int = {

    @tailrec
    def searchElement[T](list: List[T], value: T)(implicit ord: Ordering[T]): Int = {
      list match {
        case Nil => -1
        case x :: xs => if (ord.equiv(value, x)) {
          elements.indexOf(value)
        } else {
          searchElement(xs, value)
        }
      }
    }

    searchElement(elements, value)(ord)
  }
}

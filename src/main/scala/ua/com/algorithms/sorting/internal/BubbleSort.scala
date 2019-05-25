package ua.com.algorithms.sorting.internal

import scala.annotation.tailrec

object BubbleSort {

 // pseudo-code for traditional solution
 /* begin BubbleSort(list)

  for all elements of list
  if list[i] > list[i+1]
  swap(list[i], list[i+1])
  end if
  end for

  return list

  end BubbleSort*/

  def sort[T](elements: List[T])(implicit ord: Ordering[T]): List[T] = {

    @tailrec
    def loop(elements: List[T], rest: List[T], sorted: List[T]): List[T] = {
      elements match {
        case Nil => sorted
        case x :: Nil => if (rest.isEmpty) x :: sorted else loop(rest, Nil, x :: sorted) //largest element ends up in sorted
        case x :: y :: xs =>
          if (ord.lt(x, y)) {
            loop(y :: xs, x :: rest, sorted)
          } else {
            loop(x :: xs, y :: rest, sorted)
          }
      }
    }
    loop(elements, Nil, Nil)
  }
}

package ua.com.sorting.internal

import scala.annotation.tailrec

object InsertionSort {

 // pseudo-code for traditional solution
 /* function insertionSort(array A)
  for i from 1 to length[A]-1 do
    value := A[i]
    j := i-1
    while j >= 0 and A[j] > value do
      A[j+1] := A[j]
      j := j-1
    done
    A[j+1] = value
  done*/

  private def less[T](i: T, j: T)(implicit ord: Ordering[T]): Boolean =  ord.lt(i, j)

  private def insert[T](element: T, elements : Seq[T])(implicit ord: Ordering[T]) :  Seq[T] = {

    def insertElement(xs: Seq[T]) : Seq[T] = xs match {
      case Nil                         => Seq(element)
      case x :: xs if less(element, x) => element :: x :: xs
      case x :: xs                     => x +: insertElement(xs)
    }

    insertElement(elements)
  }

  def sortOne[T](elements: Seq[T])(implicit ord: Ordering[T])  : Seq[T] = {
    @tailrec
    def insertionSorting(toSort: Seq[T], sorted: Seq[T]): Seq[T] = toSort match {
      case Nil     => sorted
      case x :: xs => insertionSorting(xs, insert(x, sorted))
    }
    insertionSorting(elements, Seq())
  }

  def sortTwo[T](list: List[T])(implicit ord: Ordering[T]): List[T] = {

    def insert(list: List[T], value: T): List[T] =
      list.span(x => less(x, value)) match {
        case (lower, upper) => lower ::: value :: upper
      }

    list.foldLeft(List.empty[T])(insert)
  }
}

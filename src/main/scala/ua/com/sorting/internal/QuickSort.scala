package ua.com.sorting.internal

object QuickSort {

  def sort[T](elements: List[T])(implicit ord: Ordering[T]): List[T] = {
    elements match {
      case Nil => Nil
      case x :: xx =>
        val (left, right) = xx.partition(ord.lt(_, x))
        sort(left) ++ (x :: sort(right))
    }
  }
}

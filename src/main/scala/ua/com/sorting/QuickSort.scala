package ua.com.sorting

/*
  Best  	      Average     	Worst
  Ω(n log(n))	  θ(n log(n))  	O(n^2)
*/

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

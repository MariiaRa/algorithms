package ua.com.sorting

object MergeSort {

  def sort[A](list: List[A])(implicit ord: Ordering[A]): List[A] = {
    val middle = list.length / 2
    if (middle == 0) {
      list
    }
    else {
      val (left, right) = list.splitAt(middle)
      merge(sort(left), sort(right))
    }
  }

  def merge[A](left: List[A], right: List[A])(implicit ord: Ordering[A]): List[A] = {

    def loop(left: List[A], right: List[A], sorted: List[A]): List[A] = (left, right) match {
      case (hLeft :: tLeft, hRight :: tRight) =>
        if (ord.lt(hLeft, hRight)) loop(tLeft, right, hLeft :: sorted) else loop(left, tRight, hRight :: sorted)
      case (hLeft :: tLeft, Nil) => loop(tLeft, Nil, hLeft :: sorted)
      case (Nil, hRight :: tRight) => loop(Nil, tRight, hRight :: sorted)
      case (Nil, Nil) => sorted
    }

    loop(left, right, Nil).reverse
  }

}

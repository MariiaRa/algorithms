package ua.com.sorting

object MergeSort {

  def sort[T](list: List[T])(implicit ord: Ordering[T]): List[T] = {
    val middle = list.length / 2
    if (middle == 0) {
      list
    }
    else {
      val (left, right) = list splitAt middle
      mergeOne(sort(left), sort(right))
    }
  }

  def mergeOne[T](left: List[T], right: List[T])(implicit ord: Ordering[T]): List[T] = {

    def loop(left: List[T], right: List[T], sorted: List[T]): List[T] = (left, right) match {
      case (hLeft :: tLeft, hRight :: tRight) =>
        if (ord.lt(hLeft, hRight)) loop(tLeft, right, hLeft :: sorted) else loop(left, tRight, hRight :: sorted)
      case (hLeft :: tLeft, Nil) => loop(tLeft, Nil, hLeft :: sorted)
      case (Nil, hRight :: tRight) => loop(Nil, tRight, hRight :: sorted)
      case (Nil, Nil) => sorted
    }

    loop(left, right, Nil).reverse
  }

  //with Streams
  def sortTwo[T](xs: Stream[T])(implicit ord: Ordering[T]): Stream[T] = {
      val middle = xs.length / 2

      if (middle == 0) {
        xs
      } else {
          def merge(left: Stream[T], right: Stream[T]): Stream[T] = (left, right) match {
              case (Stream.Empty, _) => right
              case (_, Stream.Empty) => left
              case (lh #:: ltail, rh #:: rtail) =>
                  if (ord.lt(lh, rh)) lh #:: merge(ltail, right)
                  else rh #:: merge(left, rtail)
          }

          val (lelt, right) = xs splitAt middle
          merge(sortTwo(lelt), sortTwo(right))
      }
  }
}

package ua.com.sorting.internal

object SelectionSort {

  // pseudo-code for traditional solution
  /*for i = 1 to n − 1 do
    min = i
      for j = i + 1 to n do
      // Find the index of the ith smallest element
        if A[j] < A[min] then
          min = j
        end if
      end for
     Swap A[min] and A[i]
  end for*/

  def sortOne[T](list: List[T])(implicit ord: Ordering[T]): List[T] = {

    def remove(element: T, elements: List[T]): List[T] =
      elements match {
        case Nil => Nil
        case x :: xs if x == element => xs
        case x :: xs => x :: remove(element, xs)
      }

    list match {
      case Nil => Nil
      case _ =>
        val min = list.min
        min :: sortOne(remove(min, list))
    }
  }

  def sortTwo[T](a: List[T])(implicit ord: Ordering[T]): List[T] = a match {
    case Nil => Nil
    case xs => minElemet(xs) :: sortTwo(xs.diff(List(minElemet(xs)))) //or xs.min
  }

  private def minElemet[T](list: List[T])(implicit ord: Ordering[T]): T = {
    list.tail.foldLeft(list.head) { (currentMin, element) =>
      if (ord.lt(element, currentMin)) element else currentMin
    }
  }
}

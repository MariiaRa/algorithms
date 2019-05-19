package ua.com.sorting.internal

//Counting sort assumes that each n of the input elements in a list has a key value ranging from 0 to k, for some integer k.
object CountingSort {

  /**
    *
    * @param elements - input list of integers
    * @param min      - min element in input list of integers
    * @param max      - max element in input list of integers
    * @return         - sorted list
    */
  def sort(elements: List[Int], min: Int, max: Int): List[Int] =
    elements
      .foldLeft(Array.fill(max - min + 1)(0)) { (arr, integer) =>   // Counting sort starts by going through input list, and for each element list[i],
        arr(integer - min) += 1                                     // it goes to the index of array that has the same value as list[i] (so it goes to array[list[i]])
        arr                                                         // and increments the value of array[list[i]] by one.
    }
      .zipWithIndex
      .reverse
      .foldLeft(List[Int]()) { case (lst, (cnt, ndx)) =>
        List.fill(cnt)(ndx + min) ::: lst
      }
}

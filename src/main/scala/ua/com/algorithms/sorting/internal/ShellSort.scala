package ua.com.algorithms.sorting.internal

object ShellSort {

  val MARCIN_GAPS = List(701, 301, 132, 57, 23, 10, 4, 1)

  //# Start with the largest gap and work down to a gap of 1
  //foreach (gap in gaps)
  //{
  //    # Do a gapped insertion sort for this gap size.
  //    # The first gap elements a[0..gap-1] are already in gapped order
  //    # keep adding one more element until the entire array is gap sorted
  //    for (i = gap; i < n; i += 1)
  //    {
  //        # add a[i] to the elements that have been gap sorted
  //        # save a[i] in temp and make a hole at position i
  //        temp = a[i]
  //        # shift earlier gap-sorted elements up until the correct location for a[i] is found
  //        for (j = i; j >= gap and a[j - gap] > temp; j -= gap)
  //        {
  //            a[j] = a[j - gap]
  //        }
  //        # put temp (the original a[i]) in its correct location
  //        a[j] = temp
  //    }
  //}

  def sortTwo[T](elements: Array[T])(implicit ord: Ordering[T]): Unit = {
    val n = elements.length

    for {
      gap <- MARCIN_GAPS
      i <- gap until n
      j <- i to gap by -gap if less(elements(j), elements(j - gap))
    } {
      swap(elements, j, j - gap)
    }
  }

  def sortOne[T](a: Array[T])(implicit ord: Ordering[T]) {
    val n = a.length
    hsort(largest(1, n), a, n)
  }

  private def largest(h: Int, n: Int): Int =
    if (h >= n / 3) h
    else largest(3 * h + 1, n)

  private def hsort[T](h: Int, a: Array[T], n: Int)(implicit ord: Ordering[T]) {
    if (h < 1) return
    for (i <- h to n - 1)
      for (j <- i to h by -h if less(a(j), a(j - h)))
        swap(a, j, j - h)
    hsort(h / 3, a, n)
  }

  private def less[T](i: T, j: T)(implicit ord: Ordering[T]): Boolean =  ord.lt(i, j)

  private def swap [T](arr: Array [T], i: Int, j: Int): Array[T] = {
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
    arr
  }
}

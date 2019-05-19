package ua.com.sorting.internal

object BucketSort {

  private def buildBuckets(min: Int, max: Int, n_buckets: Int): Array[List[Int]] = {
    Array.fill(n_buckets)(List.empty[Int])
  }

  def sort(elements: Array[Int], n_buckets: Int): List[Int] = {
    val max = elements.max
    val min = elements.min

    val buckets = buildBuckets(min, max, n_buckets)

    for (element <- elements) {
      val bucket_number = math.min((element - min) * n_buckets / (max - min), n_buckets - 1)
      buckets(bucket_number) ::= element
    }

    buckets.foldLeft(List.empty[Int]){ case (list, bucket) =>
      list ::: InsertionSort.sortOne(bucket).toList
    }
  }
}

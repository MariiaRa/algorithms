package ua.com.sorting.internal

object HeapSort {

  // // If left child is larger than root
  //    if (l < n && arr[l] > arr[largest])
  //        largest = l;
  //
  //    // If right child is larger than largest so far
  //    if (r < n && arr[r] > arr[largest])
  //        largest = r;
  //
  //    // If largest is not root
  //    if (largest != i)
  //    {
  //        swap(arr[i], arr[largest]);
  //
  //        // Recursively heapify the affected sub-tree
  //        heapify(arr, n, largest);
  //    }

  private def swap [T](arr: Array [T], i: Int, j: Int): Array[T] = {
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
    arr
  }

  // To heapify a subtree rooted with node i which is an index in array
  private def heapify[T](elements: Array[T],  heapRoot: Int, heapSize: Int)(implicit ord: Ordering[T]): Array[T] = {
    val rootIndex = heapRoot
    val leftChildIndex = (heapRoot + 1) * 2 - 1
    val rightChildIndex = (heapRoot + 1) * 2
    (elements.lift(leftChildIndex), elements.lift(rootIndex), elements.lift(rightChildIndex)) match {
        case (Some(leftChild), Some(root), Some(rightChild)) if ord.gt(rightChild, leftChild) && ord.gt(rightChild, root) && heapSize >= rightChildIndex =>
          heapify(swap(elements, rightChildIndex, rootIndex), rightChildIndex, heapSize)

        case (Some(leftChild), Some(root), _) if ord.gt(leftChild, root) && heapSize >= leftChildIndex =>
          heapify(swap(elements, leftChildIndex, rootIndex), leftChildIndex, heapSize)

        case _ => elements
      }
  }

  //A max-heap is a complete binary tree in which the value in each internal node
  //is greater than or equal to the values in the children of that node.
  // e.g, (6, 5, 2, 3, 1)
  def buildHeap[T](elements: Array[T])(implicit ord: Ordering[T]): Array[T] = {
    (Math.floor(elements.size / 2).toInt - 1 to 0 by -1)
      .foldLeft(elements)(heapify(_, _, elements.size))
  }

  def sort[T](elements: Array[T])(implicit ord: Ordering[T]): Array[T] = {
    (elements.size - 1 to 0 by -1)
      .foldLeft(buildHeap(elements)){ (array, index) =>
        heapify(swap(array, 0, index), 0, index - 1) // At this point, the largest item is stored at the root of the heap. Replace it with the last item of the heap followed by reducing the size of heap by 1.
      }
  }
}

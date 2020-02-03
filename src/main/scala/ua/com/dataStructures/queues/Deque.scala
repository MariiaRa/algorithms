package ua.com.dataStructures.queues

//the last element of the queue is the first element of R
case class Deque[+A](outLen: Int, out: Stream[A], inLen: Int, in: Stream[A], c: Int = 2) {

  def isEmpty: Boolean = out.isEmpty && in.isEmpty

  def pushFront[B >: A](elem: B) = {
    rebalance(outLen+1, Stream.cons(elem, out), inLen, in, c)
  }

  def pushLast[B >: A](elem: B) = {
    rebalance(outLen, out, inLen+1, Stream.cons(elem, in), c)
  }

  def popFront() : (A, Deque[A]) = {
    out match {
      case Stream.Empty if in.isEmpty => throw new IllegalArgumentException("Empty queue")
      case Stream.Empty if !in.isEmpty => (in.head, rebalance(outLen, out, inLen - 1, in.drop(1), c))
      case x #:: newOut => (x, rebalance(outLen - 1, newOut, inLen, in, c))
    }
  }

  def popLast() : (A, Deque[A]) = {
    in match {
      case Stream.Empty if out.isEmpty => throw new IllegalArgumentException("Empty queue")
      case Stream.Empty if !out.isEmpty => (out.head, rebalance(outLen-1, out.tail, inLen, in, c))
      case x #:: newIn => (x, rebalance(outLen, out, inLen-1, newIn, c))
    }
  }

  private def rebalance[B >: A](outLen: Int, out: Stream[B], inLen: Int, in: Stream[B], c: Int): Deque[B] = {
    if (outLen > c*inLen+1) {
      val newOutLen = (outLen+inLen)/2
      val newInLen  = outLen + inLen - newOutLen
      val newOut = out.take(newOutLen)
      val newIn = in append out.drop(newInLen).reverse
      Deque(newOutLen, newOut, newInLen, newIn, c)
    } else if (inLen > c*outLen+1) {
      val newInLen = (outLen+inLen)/2
      val newOutLen = outLen + inLen - newInLen
      val newIn = in.take(newInLen)
      val newOut = out append in.drop(newOutLen).reverse
      Deque(newOutLen, newOut, newInLen, newIn, c)
    } else
      Deque(outLen, out, inLen, in, c)
  }

  override def toString = (in.toList ::: out.toList.reverse).mkString("Deque(", ",", ")")

}

object Deque {

  def empty[A]: Deque[A] = new Deque(0, Stream.Empty, 0, Stream.Empty)

  def apply[A](xs: A*): Deque[A] = {
    val elements = xs.toList
    val (f, r) = elements.splitAt(elements.length / 2)
    new Deque[A](f.length, f.toStream, r.length, r.reverse.toStream)
  }
}




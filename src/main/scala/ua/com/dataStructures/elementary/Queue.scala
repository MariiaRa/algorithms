package ua.com.dataStructures.elementary

class Queue[+A](front: List[A] = Nil, rear: List[A] = Nil) {
  /**
    * Check whether the queue is empty or not
    */
  def isEmpty: Boolean = front.isEmpty && rear.isEmpty

  /** Returns the length of the queue.
    */
  def length = front.length + rear.length

  /** Returns a tuple with the first element in the queue,
    * and a new queue with this element removed.
    *
    * @throws java.util.NoSuchElementException
    * @return the first element of the queue
    * Time - O(1)
    * Space - O(1)
    */
  def dequeue: (A, Queue[A]) = rear match {
    case Nil if !front.isEmpty => val rev = front.reverse; (rev.head, new Queue(Nil, rev.tail))
    case x :: xs => (x, new Queue(front, xs))
    case _ => throw new NoSuchElementException("dequeue on empty queue")
  }

  /** Creates a new queue with element added at the end
    * of the old queue
    *
    * @param  elem the element to insert
    *              Time - O(1)
    *              Space - O(1)
    */
  def enqueue[B >: A](elem: B) = new Queue(elem :: front, rear)

  /**
    * Returns the first element of the queue
    *
    * @throws java.util.NoSuchElementException
    * @return the first element of the queue
    * Time - O(1)
    * Space - O(1)
    */
  def head: A =
    if (rear.nonEmpty) rear.head
    else if (front.nonEmpty) front.last
    else throw new NoSuchElementException("head on empty queue")

  /**
    * Returns the rear of the queue
    *
    * Time - O(1)
    * Space - O(1)
    */
  def tail: Queue[A] =
    if (rear.nonEmpty) new Queue(front, rear.tail)
    else if (front.nonEmpty) new Queue(Nil, front.reverse.tail)
    else throw new NoSuchElementException("tail on empty queue")

  /** Returns a string representation of the queue
    */
  override def toString = (rear ::: front.reverse).mkString("Queue(", ",", ")")

}

object Queue {

  /**
    * Creates a new empty queue.
    *
    * Time - O(1)
    * Space - O(1)
    */
  def empty[A]: Queue[A] = new Queue(Nil, Nil)

  /**
    * Creates a new queue fromm given elements.
    *
    * Time - O(n)
    * Space - O(1)
    */
  def apply[A](xs: A*) = new Queue[A](Nil, xs.toList)
}

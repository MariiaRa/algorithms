package ua.com.dataStructures.elementary

class Queue[+A](front: List[A] = Nil, rear: List[A] = Nil) {

  def isEmpty: Boolean = front.isEmpty && rear.isEmpty

  def length = front.length + rear.length

  def dequeue: (A, Queue[A]) = rear match {
    case Nil if !front.isEmpty => val rev = front.reverse; (rev.head, new Queue(Nil, rev.tail))
    case x :: xs => (x, new Queue(front, xs))
    case _ => throw new NoSuchElementException("dequeue on empty queue")
  }

  def enqueue[B >: A](elem: B) = new Queue(elem :: front, rear)

  def head: A =
    if (rear.nonEmpty) rear.head
    else if (front.nonEmpty) front.last
    else throw new NoSuchElementException("head on empty queue")


  def tail: Queue[A] =
    if (rear.nonEmpty) new Queue(front, rear.tail)
    else if (front.nonEmpty) new Queue(Nil, front.reverse.tail)
    else throw new NoSuchElementException("tail on empty queue")

  override def toString = (rear ::: front.reverse).mkString("Queue(", ",", ")")

}

object Queue {

  def empty[A]: Queue[A] = new Queue(Nil, Nil)

  def apply[A](xs: A*) = new Queue[A](Nil, xs.toList)
}

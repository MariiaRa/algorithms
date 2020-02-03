package ua.com.dataStructures.elementary

//  Immutable Stack Type using List
class Stack[+A](elements: List[A]) {

  def size : Int = elements.length

  def top: Option[A] = if (!isEmpty) Some(elements.head) else None

  def isEmpty: Boolean = elements.isEmpty

  def pop: (Option[A], Stack[A]) = if (elements.isEmpty) (None, this) else (Some(elements.head), new Stack(elements.tail))

  def push[B >: A](x: B): Stack[B] = new Stack(x :: elements)
}

object Stack {
  def apply[A](elements: List[A] = List.empty[A]) = new Stack(elements)
}
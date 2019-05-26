package ua.com.dataStructures.elementary

//  Immutable Stack Type using List
class Stack[+A](elements: List[A]) {

  /**
    * The size of stack.
    */
  def size : Int = elements.length

  /**
    * The top of stack.
    */
  def top: Option[A] = if (!isEmpty) Some(elements.head) else None

  /**
    * Checks whether this stack is empty or not.
    */
  def isEmpty: Boolean = elements.isEmpty

  /**
    * Pops top element from this stack.
    *
    * Time - O(1)
    * Space - O(1)
    */
  def pop: (Option[A], Stack[A]) = if (elements.isEmpty) (None, this) else (Some(elements.head), new Stack(elements.tail))

  /**
    * Pushes given element into the stack.
    *
    * Time - O(1)
    * Space - O(1)
    */
  def push[B >: A](x: B): Stack[B] = new Stack(x :: elements)
}

object Stack {
  def apply[A](elements: List[A] = List.empty[A]) = new Stack(elements)
}
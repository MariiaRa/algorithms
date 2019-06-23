package ua.com.dataStructures.trees

/**
  * A color for RB-Tree's nodes.
  */
abstract sealed class Color
case object Red extends Color
case object Black extends Color

/**
  * A Red-Black Tree.
  */
abstract sealed class RBTree[+A] {

  /**
    * The color of this tree.
    */
  def color: Color

  /**
    * The value of this tree.
    */
  def value: A

  /**
    * The left child of this tree.
    */
  def left: RBTree[A]

  /**
    * The right child of this tree.
    */
  def right: RBTree[A]

  /**
    * Checks whether this tree is empty or not.
    */
  def isEmpty: Boolean

  /**
    * Adds given element 'x' into this tree.
    *
    * Time - O(log n)
    * Space - O(log n)
    */
  def add[B >: A](x: B)(implicit ordering: Ordering[B]): RBTree[B] = {
    import ordering._
    def balancedAdd(t: RBTree[A]): RBTree[B] =
      if (t.isEmpty) RBTree.make(Red, x)
      else if (x < t.value) balanceLeft(t.color, t.value, balancedAdd(t.left), t.right)
      else if (x > t.value) balanceRight(t.color, t.value, t.left, balancedAdd(t.right))
      else t

    def balanceLeft(c: Color, x: A, l: RBTree[B], r: RBTree[A]) = (c, l, r) match {
      case (Black, RBNode(Red, y, RBNode(Red, z, a, b), c), d) =>
        RBTree.make(Red, y, RBTree.make(Black, z, a, b), RBTree.make(Black, x, c, d))
      case (Black, RBNode(Red, z, a, RBNode(Red, y, b, c)), d) =>
        RBTree.make(Red, y, RBTree.make(Black, z, a, b), RBTree.make(Black, x, c, d))
      case _ => RBTree.make(c, x, l, r)
    }

    def balanceRight(c: Color, x: A, l: RBTree[A], r: RBTree[B]) = (c, l, r) match {
      case (Black, a, RBNode(Red, y, b, RBNode(Red, z, c, d))) =>
        RBTree.make(Red, y, RBTree.make(Black, x, a, b), RBTree.make(Black, z, c, d))
      case (Black, a, RBNode(Red, z, RBNode(Red, y, b, c), d)) =>
        RBTree.make(Red, y, RBTree.make(Black, x, a, b), RBTree.make(Black, z, c, d))
      case _ => RBTree.make(c, x, l, r)
    }

    def blacken(t: RBTree[B]) = RBTree.make(Black, t.value, t.left, t.right)

    blacken(balancedAdd(this))
  }
  //TODO
  def remove[B >: A](x: B)(implicit ordering: Ordering[B]): RBTree[B]

  /**
    * Checks whether this tree contains element 'x' or not.
    *
    * Time - O(log n)
    * Space - O(log n)
    */
  def member[B >: A](x: B)(implicit ordering: Ordering[B]): Boolean = {
    import ordering._
    def loop(t: RBTree[A], c: Option[A]): Boolean =
      if (t.isEmpty) check(c)
      else if (x < t.value) loop(t.left, c)
      else loop(t.right, Some(t.value))

    def check(c: Option[A]): Boolean = c match {
      case Some(y) if y == x => true
      case _ => false
    }

    loop(this, None)
  }

  def height: Int =
    if (isEmpty) 0
    else math.max(left.height, right.height) + 1

  /**
    * Fails with given message.
    */
  def fail(m: String) = throw new NoSuchElementException(m)
}

case class RBNode[A](
    color: Color,
    value: A,
    left: RBTree[A],
    right: RBTree[A]) extends RBTree[A] {
  def isEmpty = false
}

case object RBLeaf extends RBTree[Nothing] {
  def color: Color = Black
  def value: Nothing = fail("An empty tree.")
  def left: RBTree[Nothing] = fail("An empty tree.")
  def right: RBTree[Nothing] = fail("An empty tree.")
  def isEmpty = true
}

object RBTree {

  /**
    * Returns an empty red-black tree instance.
    *
    * Time - O(1)
    * Space - O(1)
    */
  def empty[A]: RBTree[A] = RBLeaf

  /**
    *
    */
  def make[A](c: Color, x: A, l: RBTree[A] = RBLeaf, r: RBTree[A] = RBLeaf): RBTree[A] =
    RBNode(c, x, l, r)

  /**
    * Creates a new red-black tree from given 'xs' sequence.
    *
    * Time - O(n log n)
    * Space - O(log n)
    */
  def apply[A](xs: A*)(implicit ordering: Ordering[A]): RBTree[A] = {
    var r: RBTree[A] = RBLeaf
    for (x <- xs) r = r.add(x)
    r
  }
}

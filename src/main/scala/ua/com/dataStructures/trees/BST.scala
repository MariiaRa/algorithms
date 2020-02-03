package ua.com.dataStructures.trees

abstract sealed class BST[+A] {
  /**
    * The value of this tree.
    */
  def value: A

  /**
    * The left child of this tree.
    */
  def left: BST[A]

  /**
    * The right child of this tree.
    */
  def right: BST[A]

  /**
    * The size of this tree.
    */
  def size: Int

  /**
    * Checks whether this tree is empty or not.
    */
  def isEmpty: Boolean

  /**
    * Checks whether this tree is balanced or not.
    *
    * Time - O(n)
    * Space - O(log n)
    */
  def isBalanced: Boolean = {
    def loop(t: BST[A]): Int =
      if (t.isEmpty) 0
      else {
        val l = loop(t.left)
        if (l == -1) -1
        else {
          val r = loop(t.right)
          if (r == -1) -1
          else if (math.abs(l - r) > 1) -1
          else 1 + math.max(l, r)
        }
      }

    !(loop(this) == -1)
  }

  /**
    * Adds given element 'x' into this tree.
    *
    * Time - O(log n)
    * Space - O(log n)
    */
  def add[B >: A](elem: B)(implicit ordering: Ordering[B]): BST[B] = {
    import ordering._
    if (isEmpty) BST.make(elem)
    else if (elem < value) BST.make(value, left.add(elem), right)
    else if (elem > value) BST.make(value, left, right.add(elem))
    else this
  }

  /**
    * Removes given element 'x' from this tree.
    *
    * Time - O(log n)
    * Space - O(log n)
    */
  def remove[B >: A](x: B)(implicit ordering: Ordering[B]): BST[B] = {
    import ordering._
    if (isEmpty) fail("Can't find " + x + " in this tree.")
    else if (x < value) BST.make(value, left.remove(x), right)
    else if (x > value) BST.make(value, left, right.remove(x))
    else {
      if (left.isEmpty && right.isEmpty) BST.empty
      else if (left.isEmpty) right
      else if (right.isEmpty) left
      else {
        val successor = right.min.asInstanceOf[B]
        BST.make(successor, left, right.remove(successor))
      }
    }
  }


  /**
    * Checks whether this tree contains element 'x' or not.
    *
    * Time - O(log n)
    * Space - O(log n)
    */
  def member[B >: A](x: B)(implicit ordering: Ordering[B]): Boolean = {
    import ordering._
    def loop(t: BST[A], c: Option[A]): Boolean =
      if (t.isEmpty) check(c)
      else if (x < t.value) loop(t.left, c)
      else loop(t.right, Some(t.value))

    def check(c: Option[A]): Boolean = c match {
      case Some(y) if y == x => true
      case _ => false
    }

    loop(this, None)
  }

  def fold[B](n: B)(op: (B, A) => B): B = {
    def loop(t: BST[A], a: B): B =
      if (t.isEmpty) a
      else loop(t.right, op(loop(t.left, a), t.value))

    loop(this, n)
  }

  /**
    *
    * A pre-order traversal first visits the node itself, then its left subtree, and eventually it's right subtree
    */
  def preorder: List[A] = {
    def loop(t: BST[A]): List[A] = t match {
      case Leaf => Nil
      case Node(v, l, r, _) => v :: (loop(l) ++ loop(r))
    }
    loop(this)
  }

  /**
    *
    * An in-order traversal visits the left subtree first, then visits the node itself, and finally visits the right subtree
    */
  def inorder: List[A] = {
    def loop(t: BST[A]): List[A] = t match {
      case Leaf => Nil
      case Node(v, l, r, _) => loop(l) ++ ( v :: loop(r))
    }
    loop(this)
  }

  /**
    *
    * A post-order traversal visits the left subtree, then visits the right subtree, and finally the node itself
    */
  def postorder: List[A] =  {
    def loop(t: BST[A]): List[A] = t match {
      case Leaf => Nil
      case Node(v, l, r, _) => loop(l) ++ loop(r) ++ List(v)
    }
    loop(this)
  }

  /**
    * Creates a new tree by mapping this tree to the 'f' function.
    *
    * Time - O(n)
    * Space - O(log n)
    */
  def map[B](f: (A) => B): BST[B] =
    if (isEmpty) BST.empty
    else BST.make(f(value), left.map(f), right.map(f))

  /**
    * Calculates the sum of all elements of this tree.
    *
    * Time - O(n)
    * Space - O(log n)
    */
  def sum[B >: A](implicit num: Numeric[B]): B = fold(num.zero)(num.plus)

  /**
    * Calculates the product of all elements of this list.
    *
    * Time - O(n)
    * Space - O(log n)
    */
  def product[B >: A](implicit num: Numeric[B]): B = fold(num.one)(num.times)

  /**
    * Searches for the minimal element of this tree.
    *
    * Time - O(log n)
    * Space - O(log n)
    */
  def min: A = {
    def loop(t: BST[A], m: A): A =
      if (t.isEmpty) m
      else loop(t.left, t.value)

    if (isEmpty) fail("An empty tree.")
    else loop(left, value)
  }

  /**
    * Searches for the maximal element of this tree.
    *
    * Time - O(log n)
    * Space - O(log n)
    */
  def max: A = {
    def loop(t: BST[A], m: A): A =
      if (t.isEmpty) m
      else loop(t.right, t.value)

    if (isEmpty) fail("An empty tree.")
    else loop(right, value)
  }

  /**
    * Calculates the height of this tree.
    *
    * Time - O(n)
    * Space - O(log n)
    */
  def height: Int =
    if (isEmpty) 0
    else 1 + math.max(left.height, right.height)

  /**
    * Calculates the depth for given element 'x'.
    *
    * Time - O(log n)
    * Space - O(log n)
    */
  def depth[B >: A](x: B)(implicit ordering: Ordering[B]): Int = {
    import ordering._
    if (isEmpty) fail("Can't find " + x + " in this tree.")
    else if (x < value) 1 + left.depth(x)
    else if (x > value) 1 + right.depth(x)
    else 0
  }

  /**
  * Converts this tree into the string representation.
  *
  * Time - O(n)
  * Space - O(log n)
  */
  override def toString: String =
    if (isEmpty) "."
    else "{ left: " + left + value + " right:" + right + "}"

 /**
  * Searches for the successor of given element 'x'.
  *
  * Time - O(log n)
  * Space - O(log n)
  */
  def successor[B >: A](x: B)(implicit ordering: Ordering[B]): A = {
    import ordering._
    def forward(t: BST[A], p: List[BST[A]]): A =
      if (t.isEmpty) fail("Can't find " + x + " in this tree.")
      else if (x < t.value) forward(t.left, t :: p)
      else if (x > t.value) forward(t.right, t :: p)
      else if (!t.right.isEmpty) t.right.min
      else backward(t, p)

    def backward(t: BST[A], p: List[BST[A]]): A =
      if (p.isEmpty) fail("The " + x + " doesn't have an successor.")
      else if (t == p.head.right) backward(p.head, p.tail)
      else p.head.value

    forward(this, Nil)
  }

 /**
  * Searches for the predecessor of given element 'x'.
  *
  * Time - O(log n)
  * Space - O(log n)
  */
  def predecessor[B >: A](x: B)(implicit ordering: Ordering[B]): A = {
    import ordering._
    def forward(t: BST[A], p: List[BST[A]]): A =
      if (t.isEmpty) fail("Can't find " + x + " in this tree.")
      else if (x < t.value) forward(t.left, t :: p)
      else if (x > t.value) forward(t.right, t :: p)
      else if (!t.left.isEmpty) t.left.max
      else backward(t, p)

    def backward(t: BST[A], p: List[BST[A]]): A =
      if (p.isEmpty) fail("The " + x + " doesn't have an predecessor.")
      else if (t == p.head.left) backward(p.head, p.tail)
      else p.head.value

    forward(this, Nil)
  }


  /**
    * Fails with given message 'm'.
    */
  def fail(m: String) = throw new NoSuchElementException(m)

}

case object Leaf extends BST[Nothing] {
  def value: Nothing = fail("An empty tree.")
  def left: BST[Nothing] = fail("An empty tree.")
  def right: BST[Nothing] = fail("An empty tree.")
  def size: Int = 0

  def isEmpty: Boolean = true
}

case class Node[+A](
     value: A,
     left: BST[A],
     right: BST[A],
     size: Int) extends BST[A] {
  def isEmpty: Boolean = false
}

object BST {

  /**
    * An empty tree.
    */
  def empty[A]: BST[A] = Leaf

  /**
    * A smart constructor for tree's branch.
    *
    * Time - O(1)
    * Space - O(1)
    */
  def make[A](x: A, l: BST[A] = Leaf, r: BST[A] = Leaf): BST[A] =
    Node(x, l, r, l.size + r.size + 1)

  /**
    * Creates a new tree from given sequence 'xs'.
    *
    * Time - O(n log n)
    * Space - O(log n)
    */
  def apply[A](xs: A*)(implicit ordering: Ordering[A]): BST[A] = {
    var r: BST[A] = BST.empty
    for (x <- xs) r = r.add(x)
    r
  }

  /**
    * Creates a new balanced tree from given sorted array 'a'.
    *
    * Time - O(n)
    * Space - O(log n)
    */
  def fromSortedArray[A](a: Array[A]): BST[A] = {
    def loop(l: Int, r: Int): BST[A] =
      if (l == r) BST.empty
      else {
        val p = (l + r) / 2
        BST.make(a(p), loop(l, p), loop(p + 1, r))
      }

    loop(0, a.length)
  }

  /**
    * Creates a new balanced tree from given sorted list 'l'.
    *
    * http://www.geeksforgeeks.org/sorted-linked-list-to-balanced-bst/
    *
    * Time - O(n)
    * Space - O(log n)
    */
  def fromSortedList[A](l: List[A]): BST[A] = {
    def loop(ll: List[A], n: Int): (List[A], BST[A]) =
      if (n == 0) (ll, BST.empty)
      else {
        val (lt, left) = loop(ll, n / 2)
        val (rt, right) = loop(lt.tail, n - 1 - n / 2)
        (rt, BST.make(lt.head, left, right))
      }

    loop(l, l.length)._2
  }

  /**
    *
    * Generates a complete tree of depth 'd' with 'x' stored in every node.
    *
    * Time - O(log n)
    * Space - O(log n)
    */
  def complete[A](x: A, d: Int): BST[A] =
    if (d == 0) BST.make(x)
    else {
      val t = BST.complete(x, d - 1)
      BST.make(x, t, t)
    }

  /**
    *
    * Generates a balanced tree of given size 's' with 'x' stored in every node.
    *
    * Time - O(log n)
    * Space - O(log n)
    */
  def balanced[A](x: A, s: Int)(implicit ordering: Ordering[A]): BST[A] = {
    def pair(ss: Int): (BST[A], BST[A]) =
      if (ss <= 0) (BST.empty, BST.empty)
      else {
        val t = balanced(x, ss - 1)
        (t, t.add(x))
      }

    val (lt, rt) = pair(s / 2)
    BST.make(x, lt, rt)
  }
}
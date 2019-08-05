package ua.com.dataStructures.trees

case object AVLLeaf extends AVLTree[Nothing] {
  def value: Nothing = fail("An empty tree.")

  def left: AVLTree[Nothing] = fail("An empty tree.")

  def right: AVLTree[Nothing] = fail("An empty tree.")

  def isEmpty: Boolean = true
}

case class AVLNode[+A](
    value: A,
    left: AVLTree[A],
    right: AVLTree[A]) extends AVLTree[A] {
  def isEmpty: Boolean = false
}


abstract sealed class AVLTree[+A] {

  def value: A

  def left: AVLTree[A]

  def right: AVLTree[A]

  def isEmpty: Boolean

  def height: Int =
    if (isEmpty) 0
    else 1 + math.max(left.height, right.height)


  override def toString: String =
    if (isEmpty) "."
    else "{ left: " + left + " value:"+ value + " right:" + right + "}"

  def fail(m: String) = throw new NoSuchElementException(m)

  def isBalanced: Boolean = {
    def loop(t: AVLTree[A]): Int =
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

  def balanceFactor: Int =
    if (isEmpty) 0
    else left.height - right.height

  def member[B >: A](x: B)(implicit ordering: Ordering[B]): Boolean = {
    import ordering._
    def loop(t: AVLTree[A], c: Option[A]): Boolean =
      if (t.isEmpty) check(c)
      else if (x < t.value) loop(t.left, c)
      else loop(t.right, Some(t.value))

    def check(c: Option[A]): Boolean = c match {
      case Some(y) if y == x => true
      case _ => false
    }

    loop(this, None)
  }

  def min: A = {
    def loop(t: AVLTree[A], m: A): A =
      if (t.isEmpty) m
      else loop(t.left, t.value)

    if (isEmpty) fail("An empty tree.")
    else loop(left, value)
  }

  def max: A = {
    def loop(t: AVLTree[A], m: A): A =
      if (t.isEmpty) m
      else loop(t.right, t.value)

    if (isEmpty) fail("An empty tree.")
    else loop(right, value)
  }

  private def leftRotate[A](t: AVLTree[A]): AVLTree[A] = t match {
    case AVLNode(x, l, AVLNode(z, m, r)) => {
      AVLNode(z, AVLNode(x, l, m), r)
    }
    case _ => fail("tree not suitable for left rotate")
  }

  private def rightRotate[A](t: AVLTree[A]): AVLTree[A] = t match {
    case AVLNode(x, AVLNode(z, l, m), r) => {
      AVLNode(z, l, AVLNode(x, m, r))
    }
    case _ => fail("tree not suitable for right rotate")
  }

  private def repair[A](t: AVLTree[A]): AVLTree[A] = {
    val tree@AVLNode(v, l, r) = t
    val balanceFactor = tree.balanceFactor

    if (balanceFactor > 1) {
      val leftBalanceFactor = l.balanceFactor
      if (leftBalanceFactor > 0) {
        // left left case
        rightRotate(tree)
      } else if (leftBalanceFactor < 0) {
        // left right case
        val newLeft = leftRotate(l)
        rightRotate(AVLNode(v, newLeft, r))
      } else tree
    } else if (balanceFactor < -1) {
      val rightBalanceFactor = r.balanceFactor
      if (rightBalanceFactor < 0) {
        // right right case
        leftRotate(tree)
      } else if (rightBalanceFactor > 0) {
        // right left case
        val newRight = rightRotate(r)
        leftRotate(AVLNode(v, l, newRight))
      } else tree
    }
    else tree
  }

  /**
    * Inserts a value into the AVL-tree.
    */
  def insert[B >: A](elem: B)(implicit ordering: Ordering[B]): AVLTree[B] = {
    import ordering._
    val tree = this match {
      case AVLLeaf => AVLTree.make(elem)
      case AVLNode(b, l, r) =>
        if (b == elem) this
        else if (b > elem) AVLNode(b, l.insert(elem), r)
        else AVLNode(b, l, r.insert(elem))
    }
    repair(tree)
  }

  private def repairDelete[A: Ordering](t: AVLTree[A]): AVLTree[A] = {
    val tree@AVLNode(v, l, r) = t
    val balanceFactor = tree.balanceFactor
    if (balanceFactor > 1) {
      val rotate = if (l.balanceFactor < 0) {
        // left right
        val newLeft = leftRotate(l)
        AVLNode(v, newLeft, r)
      } else tree // left left
      rightRotate(rotate)
    } else if (balanceFactor < -1) {
      val rotate = if (r.balanceFactor < 0) {
        // right left
        val newRight = rightRotate(r)
        AVLNode(v, l, newRight)
      } else tree // right right
      leftRotate(rotate)
    } else tree
  }

  def remove[B >: A](elem: B)(implicit ordering: Ordering[B]): AVLTree[B] = {
    import ordering._
    val tree = this match {
      case AVLLeaf => fail("Can't find " + elem + " in this tree.")
      case c@AVLNode(b, AVLLeaf, AVLLeaf) =>
        if (b == elem) AVLLeaf
        else c
      case c@AVLNode(b, l, AVLLeaf) =>
        if (b == elem) l
        else if (b < elem) c
        else AVLNode(b, l.remove(elem), AVLLeaf)
      case c@AVLNode(b, AVLLeaf, r) =>
        if (b == elem) r
        else if (b > elem) c
        else AVLNode(b, AVLLeaf, r.remove(elem))
      case AVLNode(b, l, r) if (b == elem) =>
        val m = r.min.asInstanceOf[B]
        val newr = r.remove(m)
        AVLNode(m, l, newr)
      case AVLNode(b, l, r) =>
        if (b > elem) AVLNode(b, l.remove(elem), r)
        else AVLNode(b, l, r.remove(elem))
    }
    if (tree == AVLLeaf) tree
    else repairDelete(tree)
  }
}


object AVLTree {

  def empty[A]: AVLTree[A] = AVLLeaf

  def make[A](x: A, l: AVLTree[A] = AVLLeaf, r: AVLTree[A] = AVLLeaf): AVLTree[A] =
    AVLNode(x, l, r)

  def apply[A](xs: A*)(implicit ordering: Ordering[A]): AVLTree[A] = {
    var r: AVLTree[A] = AVLTree.empty
    for (x <- xs) r = r.insert(x)
    r
  }
}
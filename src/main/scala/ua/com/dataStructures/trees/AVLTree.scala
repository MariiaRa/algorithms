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

  /**
    * Fails with given message 'm'.
    */
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

  def balance[A](t: AVLTree[A]): Int = {
    case AVLLeaf => 0
    case AVLNode(_, l, r) => l.height - r.height
  }

  private def isAVLInvariant[A: Ordering](t: AVLTree[A]): Boolean =
    t match {
      case AVLLeaf => true
      case AVLNode(_, AVLLeaf, AVLLeaf) => true
      case AVLNode(v, l, r) => {
        val ord = implicitly[Ordering[A]]
        val b = balance(t)
        val balanced = (b <= 1) && (-1 <= b)
        val leftOkay = l match {
          case AVLLeaf => true
          case AVLNode(_, _, _) => isAVLInvariant(l) &&
            ord.lt(l.max, v)
        }
        val rightOkay = r match {
          case AVLLeaf => true
          case AVLNode(_, _, _) => isAVLInvariant(r) &&
            ord.gt(r.min, v)
        }

        balanced && leftOkay && rightOkay
      }
    }

  def member[B >: A](x: B)(implicit ordering: Ordering[B]): Boolean = {
    import ordering._
    require(isAVLInvariant(this), "Tree is not an AVL tree!")

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

  def add[B >: A](elem: B)(implicit ordering: Ordering[B]): AVLTree[B] = {
    import ordering._
    if (isEmpty) AVLTree.make(elem)
    else if (elem < value) AVLTree.make(value, left.add(elem), right)
    else if (elem > value) AVLTree.make(value, left, right.add(elem))
    else this
  }

  private def leftRotate[A](t: AVLTree[A]): AVLTree[A] = t match {
    case AVLNode(x, l, AVLNode(z, m, r)) => {
      AVLNode(z, AVLNode(x, l, m), r)
    }
    case _ => sys.error("tree not suitable for left rotate!")
  }

  private def rightRotate[A](t: AVLTree[A]): AVLTree[A] = t match {
    case AVLNode(x, AVLNode(z, l, m), r) => {
      AVLNode(z, l, AVLNode(x, m, r))
    }
    case _ => sys.error("tree not suitable for right rotate!")
  }

  private def repair[A](t: AVLTree[A]): AVLTree[A] = {
    val tree@AVLNode(v, l, r) = t
    val balanceFactor = balance(tree)
    if (balanceFactor == 2) {
      // insertion into the left child
      val leftBalanceFactor = balance(l)
      if (leftBalanceFactor > 0) {
        // left left case
        rightRotate(tree)
      } else if (leftBalanceFactor < 0) {
        // left right case
        val newl = leftRotate(l)
        rightRotate(AVLNode(v, newl, r))
      } else tree
    } else if (balanceFactor == -2) {
      // insertion into the right child
      val rbal = balance(r)
      if (rbal < 0) {
        // right right case
        leftRotate(tree)
      } else if (rbal > 0) {
        // right left case
        val newr = rightRotate(r)
        leftRotate(AVLNode(v, l, newr))
      } else tree
    }
    else tree
  }

  /**
    * Inserts a value into the AVL-tree.
    */
  def insert[B >: A](elem: B)(implicit ordering: Ordering[B]): AVLTree[B] = {
    import ordering._
    require(isAVLInvariant(this), "Tree is not AVL")
    val tree = this match {
      case AVLLeaf => AVLTree.make(elem)
      case AVLNode(b, l, r) => {
        if (b == elem) this
        else if (b > elem) AVLNode(b, l.insert(elem), r)
        else AVLNode(b, l, r.insert(elem))
      }
    }
    repair(tree)
  } ensuring (r => isAVLInvariant(r) && r.member(elem))

  private def repairDelete[A: Ordering](t: AVLTree[A]): AVLTree[A] = {
    val tree@AVLNode(v, l, r) = t
    val balanceFactor = balance(tree)
    if (balanceFactor == 2) {
      // deletion in right subtree
      val rotate = if (balance(l) < 0) {
        // left right
        val newL = leftRotate(l)
        AVLNode(v, newL, r)
      } else tree // left left
      rightRotate(rotate)
    } else if (balanceFactor == -2) {
      // deletion in left subtree
      val rotate = if (balance(r) < 0) {
        // right left
        val newR = rightRotate(r)
        AVLNode(v, l, newR)
      } else tree // right right
      leftRotate(rotate)
    } else tree
  } ensuring (r => isAVLInvariant(r))

  def remove[B >: A](elem: B)(implicit ordering: Ordering[B]): AVLTree[B] = {
    require(isAVLInvariant(this), "Tree is not AVL")
    import ordering._
    val tree = this match {
      case AVLLeaf => fail("Can't find " + elem + " in this tree.")
      // removing leafs
      case c@AVLNode(b, AVLLeaf, AVLLeaf) => {
        if (b == elem) AVLLeaf
        else c
      }
      // removing nodes with just one child
      case c@AVLNode(b, l, AVLLeaf) => {
        if (b == elem) l
        else if (b < elem) c
        else AVLNode(b, l.remove(elem), AVLLeaf)
      }
      case c@AVLNode(b, AVLLeaf, r) => {
        if (b == elem) r
        else if (b > elem) c
        else AVLNode(b, AVLLeaf, r.remove(elem))
      }
      // full on deletion
      case AVLNode(b, l, r) if (b == elem) => {
        // get left most child of the right tree
        val m = r.min
        val newr = r.remove(m)
        AVLNode(m, l, newr)
      }
      case AVLNode(b, l, r) => {
        if (b > elem) AVLNode(b, l.remove(elem), r)
        else AVLNode(b, l, r.remove(elem))
      }
    }
    if (tree == Left) tree
    else repairDelete(tree)
  } ensuring (r => isAVLInvariant(r) && !r.member(elem))
}


object AVLTree {

  def empty[A]: AVLTree[A] = AVLLeaf

  def make[A](x: A, l: AVLTree[A] = AVLLeaf, r: AVLTree[A] = AVLLeaf): AVLTree[A] =
    AVLNode(x, l, r)

  def apply[A](xs: A*)(implicit ordering: Ordering[A]): AVLTree[A] = {
    var r: AVLTree[A] = AVLTree.empty
    for (x <- xs) r = r.add(x)
    r
  }
}
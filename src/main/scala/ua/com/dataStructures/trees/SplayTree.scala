package ua.com.dataStructures.trees

abstract sealed class SplayTree[+A] {

  def value: A

  def left: SplayTree[A]

  def right: SplayTree[A]

  def isEmpty: Boolean

  def isBalanced: Boolean = {
    def loop(t: SplayTree[A]): Int =
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

  def add[B >: A](elem: B)(implicit ordering: Ordering[B]): SplayTree[B] = {
    import ordering._
    if (isEmpty) SplayTree.make(elem)
    else if (elem < value) {
      SplayTree.make(value, left.add(elem), right) splay elem
    }
    else if (elem > value) {
      SplayTree.make(value, left, right.add(elem)) splay elem
    }
    else this
  }

  def remove[B >: A](x: B)(implicit ordering: Ordering[B]): SplayTree[B] = {
    import ordering._
    if (isEmpty) fail("Can't find " + x + " in this tree.")
    else if (x < value) SplayTree.make(value, left.remove(x), right)
    else if (x > value) SplayTree.make(value, left, right.remove(x))
    else {
      if (left.isEmpty && right.isEmpty) SplayTree.empty
      else if (left.isEmpty) right
      else if (right.isEmpty) left
      else {
        val successor = right.min.asInstanceOf[B]
        SplayTree.make(successor, left, right.remove(successor))
      }
    }
  }

  def member[B >: A](x: B)(implicit ordering: Ordering[B]): Boolean = {
    import ordering._
    def loop(t: SplayTree[A], c: Option[A]): Boolean =
      if (t.isEmpty) check(c)
      else if (x < t.value) loop(t.left, c)
      else loop(t.right, Some(t.value))
    def check(c: Option[A]): Boolean = c match {
      case Some(y) if y == x => true
      case _ => false
    }
    loop(this, None)
  }

  def preorder: List[A] = {
    def loop(t: SplayTree[A]): List[A] = t match {
      case SplayLeaf => Nil
      case SplayNode(v, l, r) => v :: (loop(l) ++ loop(r))
    }
    loop(this)
  }

  def inorder: List[A] = {
    def loop(t: SplayTree[A]): List[A] = t match {
      case SplayLeaf => Nil
      case SplayNode(v, l, r) => loop(l) ++ (v :: loop(r))
    }
    loop(this)
  }

  def postorder: List[A] = {
    def loop(t: SplayTree[A]): List[A] = t match {
      case SplayLeaf => Nil
      case SplayNode(v, l, r) => loop(l) ++ loop(r) ++ List(v)
    }
    loop(this)
  }

  def min: A = {
    def loop(t: SplayTree[A], m: A): A =
      if (t.isEmpty) m
      else loop(t.left, t.value)
    if (isEmpty) fail("An empty tree.")
    else loop(left, value)
  }

  def max: A = {
    def loop(t: SplayTree[A], m: A): A =
      if (t.isEmpty) m
      else loop(t.right, t.value)
    if (isEmpty) fail("An empty tree.")
    else loop(right, value)
  }

  def height: Int =
    if (isEmpty) 0
    else 1 + math.max(left.height, right.height)

  def depth[B >: A](x: B)(implicit ordering: Ordering[B]): Int = {
    import ordering._
    if (isEmpty) fail("Can't find " + x + " in this tree.")
    else if (x < value) 1 + left.depth(x)
    else if (x > value) 1 + right.depth(x)
    else 0
  }

  override def toString: String =
    if (isEmpty) "."
    else "{ left: " + left + value + " right:" + right + "}"

  def successor[B >: A](x: B)(implicit ordering: Ordering[B]): A = {
    import ordering._
    def forward(t: SplayTree[A], p: List[SplayTree[A]]): A =
      if (t.isEmpty) fail("Can't find " + x + " in this tree.")
      else if (x < t.value) forward(t.left, t :: p)
      else if (x > t.value) forward(t.right, t :: p)
      else if (!t.right.isEmpty) t.right.min
      else backward(t, p)
    def backward(t: SplayTree[A], p: List[SplayTree[A]]): A =
      if (p.isEmpty) fail("The " + x + " doesn't have an successor.")
      else if (t == p.head.right) backward(p.head, p.tail)
      else p.head.value
    forward(this, Nil)
  }

  def predecessor[B >: A](x: B)(implicit ordering: Ordering[B]): A = {
    import ordering._
    def forward(t: SplayTree[A], p: List[SplayTree[A]]): A =
      if (t.isEmpty) fail("Can't find " + x + " in this tree.")
      else if (x < t.value) forward(t.left, t :: p)
      else if (x > t.value) forward(t.right, t :: p)
      else if (!t.left.isEmpty) t.left.max
      else backward(t, p)
    def backward(t: SplayTree[A], p: List[SplayTree[A]]): A =
      if (p.isEmpty) fail("The " + x + " doesn't have an predecessor.")
      else if (t == p.head.left) backward(p.head, p.tail)
      else p.head.value
    forward(this, Nil)
  }

  private def splay[B >: A](key: B)(implicit ordering: Ordering[B]): SplayTree[B] = {
    import ordering._

    def loop(tree: SplayTree[B], elem: B): SplayTree[B] = {
      if (tree.value > elem) { //check left sub-tree
        tree match {
          // zig-zig left
          case tree@SplayNode(g, SplayNode(p, SplayNode(x, a, b), c), d) =>
            if (x == elem) {
              SplayNode(x, a, SplayNode(p, b, SplayNode(g, c, d)))
            } else {
              tree
            }
          // zig-zag left
          case tree@SplayNode(g, SplayNode(p, a, SplayNode(x, b, c)), d) =>
            if (x == elem) {
              SplayNode(x, SplayNode(p, a, b), SplayNode(g, c, d))
            } else {
              tree
            }
          // zig left
          case tree@SplayNode(p, SplayNode(x, a, b), c) =>
            if (x == elem) {
              SplayNode(x, a, SplayNode(p, b, c))
            } else {
              tree
            }
          case _ => tree
        }
      } else if (tree.value < elem) { //check right sub-tree
        tree match {
          // zig-zig right
          case tree@SplayNode(g, a, SplayNode(p, b, SplayNode(x, c, d))) =>
            if (x == elem) {
              SplayNode(x, SplayNode(p, SplayNode(g, a, b), c), d)
            } else {
              tree
            }
          // zig-zag right
          case tree@SplayNode(g, a, SplayNode(p, SplayNode(x, b, c), d)) =>
            if (x == elem) {
              SplayNode(x, SplayNode(g, a, b), SplayNode(p, c, d))
            } else {
              tree
            }
          // zig right
          case tree@SplayNode(p, a, SplayNode(x, b, c)) =>
            if (x == elem) {
              SplayNode(x, SplayNode(p, a, b), c)
            } else {
              tree
            }
          case _ => tree
        }
      } else {
        tree
      }
    }
    loop(this, key)
  }


  def join[B >: A](that: SplayTree[B])(implicit ordering: Ordering[B]): SplayTree[B] = {
    this match {
      case SplayLeaf => that
      case SplayNode(x, l, r) =>
        val (_l, _r) = that.split(x)
        SplayNode(x, l join _l, r join _r)
    }
  }


  def split[B >: A](pivot: B)(implicit ordering: Ordering[B]): (SplayTree[B], SplayTree[B]) = {
    //step 1 splay pivot
    if (isEmpty) (SplayLeaf, SplayLeaf)
    else {
      val SplayNode(x, l, r) = this splay pivot
      (SplayNode(x, l, SplayLeaf), r)
    }
  }
  def fail(m: String) = throw new NoSuchElementException(m)
}


case object SplayLeaf extends SplayTree[Nothing] {
  def value: Nothing = fail("An empty tree.")
  def left: SplayTree[Nothing] = fail("An empty tree.")
  def right: SplayTree[Nothing] = fail("An empty tree.")
  def isEmpty: Boolean = true
}

case class SplayNode[+A](
    value: A,
    left: SplayTree[A],
    right: SplayTree[A]) extends SplayTree[A] {
  def isEmpty: Boolean = false
}

object SplayTree {

  def empty[A]: SplayTree[A] = SplayLeaf

  def make[A](x: A, l: SplayTree[A] = SplayLeaf, r: SplayTree[A] = SplayLeaf): SplayTree[A] =
    SplayNode(x, l, r)

  def apply[A](xs: A*)(implicit ordering: Ordering[A]): SplayTree[A] = {
    var r: SplayTree[A] = SplayTree.empty
    for (x <- xs) r = r.add(x)
    r
  }

  def fromSortedArray[A](a: Array[A]): SplayTree[A] = {
    def loop(l: Int, r: Int): SplayTree[A] =
      if (l == r) SplayTree.empty
      else {
        val p = (l + r) / 2
        SplayTree.make(a(p), loop(l, p), loop(p + 1, r))
      }
    loop(0, a.length)
  }

  def fromSortedList[A](l: List[A]): SplayTree[A] = {
    def loop(ll: List[A], n: Int): (List[A], SplayTree[A]) =
      if (n == 0) (ll, SplayTree.empty)
      else {
        val (lt, left) = loop(ll, n / 2)
        val (rt, right) = loop(lt.tail, n - 1 - n / 2)
        (rt, SplayTree.make(lt.head, left, right))
      }
    loop(l, l.length)._2
  }

  def complete[A](x: A, d: Int): SplayTree[A] =
    if (d == 0) SplayTree.make(x)
    else {
      val t = SplayTree.complete(x, d - 1)
      SplayTree.make(x, t, t)
    }

  def balanced[A](x: A, s: Int)(implicit ordering: Ordering[A]): SplayTree[A] = {
    def pair(ss: Int): (SplayTree[A], SplayTree[A]) =
      if (ss <= 0) (SplayTree.empty, SplayTree.empty)
      else {
        val t = balanced(x, ss - 1)
        (t, t.add(x))
      }
    val (lt, rt) = pair(s / 2)
    SplayTree.make(x, lt, rt)
  }
}



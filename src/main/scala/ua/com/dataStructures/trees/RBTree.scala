package ua.com.dataStructures.trees

import ua.com.dataStructures.trees

/**
  * A color for RB-Tree's nodes.
  */
abstract sealed class Color

case object Red extends Color

case object Black extends Color

//By adding the color double-black, the hard case reduces to changing the target node into a double-black leaf.
// A double-black node counts twice for black height, which allows the black-height invariant to be preserved.
//For instance, adding a black to a black yields a double-black.
// Subtracting a black from a black yields a red.
// Subtracting a black from a red yields a negative black.
case object BlackBlack extends Color // double black

case object NegativeBlack extends Color // negative black

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


  def insert[B >: A](x: B)(implicit ordering: Ordering[B]): RBTree[B] = {
    import ordering._
    def balancedAdd(t: RBTree[A]): RBTree[B] = {
      if (t.isEmpty) RBTree.make(Red, x)
      else if (x < t.value) _balance(t.color, t.value, balancedAdd(t.left), t.right)
      else if (x > t.value) _balance(t.color, t.value, t.left, balancedAdd(t.right))
      else t
    }

    _blacken(balancedAdd(this))
  }


  def delete[B >: A](x: B)(implicit ordering: Ordering[B]): RBTree[B] = {
    import ordering._
   //The procedure del searches through the tree until it finds the node to delete, and then it calls remove
    def del(t: RBTree[A]): RBTree[A] = {
      println(s"tree inside del: $t, value to remove: $x")
      if (t.isEmpty) RBLeaf
      if (x < t.value) bubble(t.color, t.value, del(t.left), t.right) //Because deletion could produce a double-black node, the procedure bubble gets invoked to move it upward.
      if (x > t.value) bubble(t.color, t.value, t.left, del(t.right))
      else remove(t)
    }
    //def blacken(t: RBTree[B]) = RBTree.make(Black, t.value, t.left, t.right)

    _blacken(del(this))
  }


  def removeMax[A](t: RBTree[A]): RBTree[A] = t match {
    case RBLeaf => fail("No maximum to remove")
    case t@RBNode(_, _, _, RBLeaf) => remove(t)
    case t@RBNode(c, x, l, r) => bubble(c, x, l, removeMax(r))
  }

  def remove[A](t: RBTree[A]): RBTree[A] = t match {
    case RBLeaf => RBLeaf
    case RBNode(Red, _, RBLeaf, RBLeaf) => RBLeaf
    case RBNode(Black, _, RBLeaf, RBLeaf) => RBBLeaf
    case RBNode(Black, _, RBLeaf, RBNode(Red, x, a, b)) => RBNode(Black, x, a, b)
    case RBNode(Black, _, RBNode(Red, x, a, b), RBLeaf) => RBNode(Black, x, a, b)
    case RBNode(c, y, l, r) => bubble(c, _max(l), removeMax(l), r)
  }

  /**
    * Adds given element 'x' into this tree.
    *
    * Time - O(log n)
    * Space - O(log n)
    */
  def add[B >: A](x: B)(implicit ordering: Ordering[B]): RBTree[B] = {
    import ordering._
    def balancedAdd(t: RBTree[A]): RBTree[B] = {
      if (t.isEmpty) RBTree.make(Red, x)
      else if (x < t.value) balanceLeft(t.color, t.value, balancedAdd(t.left), t.right)
      else if (x > t.value) balanceRight(t.color, t.value, t.left, balancedAdd(t.right))
      else t
    }

    def balanceLeft(c: Color, x: A, l: RBTree[B], r: RBTree[A]) = (c, l, r) match {
      case (Black, RBNode(Red, y, RBNode(Red, z, a, b), c), d) =>
        RBTree.make(Red, y, RBTree.make(Black, z, a, b), RBTree.make(Black, x, c, d))
      case (Black, RBNode(Red, z, a, RBNode(Red, y, b, c)), d) =>
        RBTree.make(Red, y, RBTree.make(Black, z, a, b), RBTree.make(Black, x, c, d))
      case _ => RBTree.make(c, x, l, r)
    }

    def balanceRight(c: Color, x: A, l: RBTree[A], r: RBTree[B]) = {
      (c, l, r) match {
        case (Black, a, RBNode(Red, y, b, RBNode(Red, z, c, d))) =>
          RBTree.make(Red, y, RBTree.make(Black, x, a, b), RBTree.make(Black, z, c, d))
        case (Black, a, RBNode(Red, z, RBNode(Red, y, b, c), d)) =>
          RBTree.make(Red, y, RBTree.make(Black, x, a, b), RBTree.make(Black, z, c, d))
        case _ => RBTree.make(c, x, l, r)
      }
    }

    def blacken(t: RBTree[B]) = RBTree.make(Black, t.value, t.left, t.right)

    blacken(balancedAdd(this))
  }

//Balancing eliminates double blacks and negative blacks at the same time.
// Okasaki's red-black algorithms use a rebalancing procedure.
// It's possible to generalize this rebalancing procedure with two new cases so that it can reliably eliminate double blacks and negative blacks.
  private def _balance[A](c: Color, z: A, l: RBTree[A], r: RBTree[A]): RBTree[A] = (c, l, z, r) match {
    // -- Okasaki's original cases:
    case (Black, RBNode(Red, y, RBNode(Red, x, a, b), c), z, d) =>
      RBTree.make(Red, y, RBTree.make(Black, x, a, b), RBTree.make(Black, z, c, d))
    case (Black, RBNode(Red, x, a, RBNode(Red, y, b, c)), z, d) =>
      RBTree.make(Red, y, RBTree.make(Black, x, a, b), RBTree.make(Black, z, c, d))
    case (Black, a, x, RBNode(Red, z, RBNode(Red, y, b, c), d)) =>
      RBTree.make(Red, y, RBTree.make(Black, x, a, b), RBTree.make(Black, z, c, d))
    case (Black, a, x, RBNode(Red, y, b, RBNode(Red, z, c, d))) =>
      RBTree.make(Red, y, RBTree.make(Black, x, a, b), RBTree.make(Black, z, c, d))
    // -- Six cases for deletion:
    case (BlackBlack, RBNode(Red, y, RBNode(Red, x, a, b), c), z, d) =>
      RBTree.make(Black, y, RBTree.make(Black, x, a, b), RBTree.make(Black, z, c, d))
    case (BlackBlack, RBNode(Red, x, a, RBNode(Red, y, b, c)), z, d) =>
      RBTree.make(Black, y, RBTree.make(Black, x, a, b), RBTree.make(Black, z, c, d))
    case (BlackBlack, a, x, RBNode(Red, z, RBNode(Red, y, b, c), d)) =>
      RBTree.make(Black, y, RBTree.make(Black, x, a, b), RBTree.make(Black, z, c, d))
    case (BlackBlack, a, x, RBNode(Red, y, b, RBNode(Red, z, c, d))) =>
      RBTree.make(Black, y, RBTree.make(Black, x, a, b), RBTree.make(Black, z, c, d))
    case (BlackBlack, a, x, RBNode(NegativeBlack, z, RBNode(BlackBlack, y, b, c), d@RBNode(Black, _, _, _))) =>
      RBTree.make(Black, y, RBTree.make(Black, x, a, b), _balance(Black, z, c, redden(d)))
    case (BlackBlack, RBNode(NegativeBlack, x, a@RBNode(Black, _, _, _), RBNode(Red, y, b, c)), z, d) =>
      RBTree.make(Black, y, _balance(Black, x, redden(a), b), RBTree.make(Black, z, c, d))
    case (c, a, x, b) => RBTree.make(c, x, a, b)
  }

  //Bubbling tries to eliminate the double black just created by a removal.
  // Sometimes, it's possible to eliminate a double-black by recoloring its parent and its sibling.
  // If that's not possible, then the double-black gets "bubbled up" to its parent.
  // To do so, it might be necessary to recolor the double black's (red) sibling to negative black.
  private def bubble[A](c: Color, x: A, l: RBTree[A], r: RBTree[A]) = {
    if (isBB(l) || isBB(r)) _balance(blacker(c), x, _redder(l), _redder(r))
    else _balance(c, x, l, r)
  }

  private def _blacken[A](t: RBTree[A]): RBTree[A] = {
    t match {
      case RBLeaf => RBLeaf
      case RBBLeaf => RBLeaf
      case RBNode(_, x, a, b) => RBTree.make(Black, t.value, t.left, t.right)
    }
  }

  private def _max[A](t: RBTree[A]): A = {
    t match {
      case RBLeaf => fail("no largest element")
      case RBNode(color, value, _, RBLeaf) => value
      case RBNode(color, value, _, r) => _max(r)
    }
  }

  def max: A = {
    def loop(t: RBTree[A], m: A): A =
      if (t.isEmpty) m
      else loop(t.right, t.value)

    if (isEmpty) fail("An empty tree.")
    else loop(right, value)
  }

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

  private def redden[A](t: RBTree[A]): RBTree[A] = {
    t match {
      case RBLeaf => fail("Cannot redden empty tree")
      case RBBLeaf => fail("Cannot redden empty tree")
      case RBNode(_, x, a, b) => RBTree.make(Red, t.value, t.left, t.right)
    }
  }

  private def isBB[A](t: RBTree[A]): Boolean = {
    t match {
      case RBBLeaf => true
      case RBNode(BlackBlack, x, a, b) => true
      case _ => false
    }
  }

  private def blacker(c: Color) = c match {
    case NegativeBlack => Red
    case Red => Black
    case Black => BlackBlack
    case BlackBlack => fail("Too black")
  }

  private def redder(c: Color) = c match {
    case NegativeBlack => fail("Not black enough")
    case Red => NegativeBlack
    case Black => Red
    case BlackBlack => Black
  }

  private def _blacker[A](t: RBTree[A]) = t match {
    case RBLeaf => RBBLeaf
    case RBNode(c, x, a, b) => RBTree.make(blacker(c), t.value, t.left, t.right)
  }

  private def _redder[A](t: RBTree[A]) = t match {
    case RBBLeaf => RBLeaf
    case RBNode(c, x, a, b) => RBTree.make(redder(c), t.value, t.left, t.right)
  }

  /**
    * Fails with given message.
    */
  def fail(m: String) = throw new NoSuchElementException(m)

  override def toString: String =
    if (isEmpty) "leaf"
    else "{left: " + left + "[value=" + value + color + "]right:" + right + "}"

  def newInsert[B >: A](x: B)(implicit ordering: Ordering[B]): RBTree[B] = {
    import ordering._
    def ins(t: RBTree[A]): RBTree[B] = {
      if (t.isEmpty) RBTree.make(Red, x)
      else if (x < t.value) newBalance(t.color, t.value, ins(t.left), t.right)
      else if (x > t.value) newBalance(t.color, t.value, t.left, ins(t.right))
      else t
    }
    def blacken(t: RBTree[B]) = RBTree.make(Black, t.value, t.left, t.right)
    blacken(ins(this))
  }

  private def newBalance[A](c: Color, z: A, l: RBTree[A], r: RBTree[A]): RBTree[A] = (c, l, z, r) match {
    //balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
    case (Black, RBNode(Red, y, RBNode(Red, x, a, b), c), z, d) => RBTree.make(Red, y, RBNode(Black, x, a, b), RBNode(Black, z, c, d)) // RBTree.make(Black, y, RBTree.make(Black, x, a, b), RBTree.make(Black, z, c, d))
    //balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
    case (Black, RBNode(Red, x, a, RBNode(Red, y, b, c)), z, d) => RBTree.make(Red, y, RBNode(Black, x, a, b), RBNode(Black, z, c, d))
    //balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
    case (Black, a, x, RBNode(Red, z, RBNode(Red, y, b, c ), d)) => RBTree.make(Red, y, RBNode(Black, x, a, b), RBNode(Black, z, c, d))
    //balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
    case (Black, a, x, RBNode(Red, y, b, RBNode(Red, z, c, d))) => RBTree.make(Red, y, RBNode(Black, x, a, b), RBNode(Black, z, c, d))
    //balance BB a x (T R (T R b y c) z d) = T B (T B a x b) y (T B c z d)
    case (BlackBlack, a, x, RBNode(Red, z, RBNode(Red, y, b, c), d)) => RBTree.make(Black, y, RBNode(Black, x, a, b), RBNode(Black, z, c, d))
    //balance BB (T R a x (T R b y c)) z d = T B (T B a x b) y (T B c z d)
    case (BlackBlack, RBNode(Red, x, a, RBNode(Red, y, b, c)), z, d) => RBTree.make(Black, y, RBNode(Black, x, a, b), RBNode(Black, z, c, d))
    //balance coloraxb=T color a x b
    case (c, a, x, b) => RBTree.make(c, x, a, b)
  }

  def newDelete[B >: A](x: B)(implicit ordering: Ordering[B]): RBTree[B] = {
    import ordering._
    def del(t: RBTree[A]): RBTree[B] = {
      t match {
        case RBLeaf => RBLeaf
        case RBNode(Red, y, RBLeaf, RBLeaf) => {
          if (x == y) {
            RBLeaf
          } else {
            RBNode(Red, y, RBLeaf, RBLeaf)
          }
        }
        case RBNode(Black, y, RBLeaf, RBLeaf) => {
          if (x == y) {
            RBBLeaf
          } else {
            RBNode(Black, y, RBLeaf, RBLeaf)
          }
        }
        case RBNode(Black, z, RBNode(Red, y, RBLeaf, RBLeaf), RBLeaf) => {
          if (x < z) {
            RBNode(Black, z, del(RBNode(Red, y, RBLeaf, RBLeaf)), RBLeaf)
          } else if (x == z) {
            RBNode(Black, y, RBLeaf, RBLeaf)
          } else {
            RBNode(Black, z, RBNode(Red, y, RBLeaf, RBLeaf), RBLeaf)
          }
        }
        case RBNode(c, y, a, b) => {
          if (x < y) {
            rotate (c, del(a), y, b)
          } else if (x == y) {
            val (_y, _b) = min_del(b)
            rotate(c, a, _y, _b)
          } else {
            rotate(c, a, y, del(b))
          }
        }
      }
    }

    def newRedden[A](t: RBTree[A]): RBTree[A] = {
      t match {
        case RBLeaf => fail("Cannot redden empty tree")
        case RBBLeaf => fail("Cannot redden empty tree")
        case RBNode(_, x, a, b) => RBTree.make(Red, t.value, t.left, t.right)
      }
    }
    del(redden(this))
  }


  def min_del[A](t: RBTree[A]): (A, RBTree[A]) = {
    t match {
      case RBNode(Red, x, RBLeaf, RBLeaf) => (x, RBLeaf)
      case RBNode(Black, x, RBLeaf, RBLeaf) => (x, RBBLeaf)
      case RBNode(Black, x, RBLeaf, RBNode(Red, y, RBLeaf, RBLeaf)) => (x, RBNode(Black, y, RBLeaf, RBLeaf))
      case RBNode(c, x, a, b) => {
        val (_x, _a) = min_del(a)
        (_x, rotate(c, _a, x, b))
    }
    }
  }









  def rotate[A](c: Color, l: RBTree[A], z: A, r: RBTree[A]): RBTree[A] = (c, l, z, r) match {
  //rotate R (T BB a x b) y (T B c z d) = balance B (T R (T B a x b) y c) z d
      case (Red, RBNode(BlackBlack, x, a, b), y, RBNode(Black, z, c,d )) => newBalance(Black, z, RBNode(Red, y, RBNode(Black, x, a, b), c), d)
      //rotate R EE y (T B c z d) = balance B (T R E y c) z d
      case (Red, RBBLeaf, y, RBNode(Black, z, c, d))  => newBalance(Black, z, RBNode(Red, y, RBLeaf, c), d)
      //rotate R (T B a x b) y (T BB c z d) = balance B a x (T R b y (T B c z d))
      case (Red,RBNode(Black, x, a, b), y, RBNode(BlackBlack, z, c,d )) => newBalance(Black, x, a, RBNode(Red, y, b, RBNode(Black, z, c,d )))
      //rotate R (T B a x b) y EE = balance B a x (T R b y E)
      case (Red, RBNode(Black, x, a, b), y, RBBLeaf) => newBalance(Black, x, a, RBNode(Red, y, b, RBLeaf))
    //rotate B (T BB a x b) y (T B c z d) = balance BB (T R (T B a x b) y c) z d
      case (Black, RBNode(BlackBlack, x, a, b), y, RBNode(Black, z, c,d )) => newBalance(BlackBlack, z, RBNode(Red, y, RBNode(Black, x, a, b), c), d)
    //rotate B EE y (T B c z d) = balance BB (T R E y c) z d
      case (BlackBlack, RBBLeaf, y, RBNode(Black, z, c, d)) => newBalance(BlackBlack, z, RBNode(Red, y, RBBLeaf, c), d)
    //rotate B (T B a x b) y (T BB c z d) = balance BB a x (T R b y (T B c z d))
      case (Black, RBNode(Black, x, a, b), y, RBNode(BlackBlack, z, c, d)) => newBalance(BlackBlack, x, a, RBNode(Red, y, b, RBNode(Black, z, c, d)))
    //rotate B (T B a x b) y EE = balance BB a x (T R b y E)
      case (Black, RBNode(Black, x, a, b), y, RBBLeaf) => newBalance(BlackBlack, x, a, RBNode(Red, y, b, RBBLeaf))
    //rotate B (T BB a w b) x (T R (T B c y d) z e) = T B (balance B (T R (T B a w b) x c) y d) z e
      case (Black, RBNode(BlackBlack, w, a, b), x, RBNode(Red, z, RBNode(Black, y, c, d), e)) => RBTree.make(Black, z, newBalance(Black, y, RBNode(Red, x, RBNode(Black, w, a, b), c), d), e)
    //rotate B EE x (T R (T B c y d) z e) = T B (balance B (T R E x c) y d) z e
      case (Black, RBBLeaf, x, RBNode(Red, z, RBNode(Black, y, c, d), e)) => newBalance(Black, z, RBNode(Red, x, RBLeaf, c) ,e)
    //rotate B (T R a w (T B b x c)) y (T BB d z e) = TBaw (balance B b x (T R c y (T B d z e)))
      case (Black, RBNode(Red, w, a, RBNode(Black, x, b, c)), y, RBNode(BlackBlack, z, d, e)) => RBTree.make(Black, w, a, newBalance(Black, x, b, RBNode(Red, y, c, RBNode(Black, z, d, e))))
    //rotate B (T R a w (T B b x c)) y EE = T B a w (balance B b x (T R c y E))
      case (Black, RBNode(Red, w, a, RBNode(Black, x, b, c)),y, RBBLeaf) => RBTree.make(Black, w, a, newBalance(Black, x, b, RBNode(Red, y, c, RBLeaf)))
    //rotate color axb=T coloraxb
      case (c, a, x, b) => RBTree.make(c, x, a, b)
  }

}

case class RBNode[+A](
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

case object RBBLeaf extends RBTree[Nothing] {
  def color: Color = BlackBlack

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
    for (x <- xs) r = r.newInsert(x)
    r
  }
}

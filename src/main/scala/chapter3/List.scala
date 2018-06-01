package chapter3

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h,l)

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case x => drop(tail(l), x-1)
  }

  def dropWhile[A](l: List[A])( f: A => Boolean): List[A] = l match {
    case _ => l
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l,0)((_,y)=>y+1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs,f(z,x))(f)
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def leftLength[A](l: List[A]): Int = foldLeft(l,0)((x,_) => x + 1)

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => foldLeft(xs, Cons(x,Nil))((agg, x)=>Cons(x,agg))
  }

  def foldAppend[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1,a2)(Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l,Nil:List[A])(foldAppend)

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l,List[B]())((x,y)=>Cons(f(x),y))

  def add1Tolist(l:List[Int]) = map(l)(_+1)

  def dToString(l:List[Double]) = map(l)(_.toString)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((x,y) => if (f(x)) Cons(x,y) else y)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as,Nil:List[B])((x,y)=> append(f(x),y))

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((x)=>if (f(x)) List(x) else List())

  def zipWith[A](l1: List[A], l2: List[A])(f:(A,A)=>A): List[A] =
    l1 match {
      case Nil => l2
      case Cons(x, xs) => l2 match {
        case Nil => Cons(x,xs)
        case Cons(y, ys) => Cons(f(x,y),zipWith(xs,ys)(f))
      }
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def matchedStart(sub:List[A], sup:List[A]): Boolean = (sub,sup) match {
      case (Cons(x,xs),Cons(y,ys)) if (x==y) => matchedStart(xs,ys)
      case (Nil,_) => true
      case(_,_) => false
    }
    sup match {
      case x if matchedStart(sub, x) => true
      case Cons(x,xs) => hasSubsequence(xs,sub)
      case _ => false
    }
  }


}
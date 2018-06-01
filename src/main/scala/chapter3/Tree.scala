package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t:Tree[A]):Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l)+size(r)+1
  }

  def maximum(t:Tree[Int]):Int = t match {
    case Leaf(v) => v
    case Branch(l,f) => maximum(l) max maximum(f)
  }

  def maxFold(t:Tree[Int]):Int = fold(t)(_ max _,x=>x)
  def sizeFold[A](t:Tree[A]):Int = fold(t)(_+_+1,_=>1)

  def depth(t:Tree[Int]):Int = t match {
    case Leaf(v) => 0
    case Branch(l,f) => depth(l) max depth(f) + 1
  }

  def depthFold[A](t:Tree[A]):Int = fold(t)((x,y)=>x.max(y) + 1 ,_=>0)

  def fold[A,B](t:Tree[A])(f:(B,B)=>B, g:A=>B):B = t match {
    case Leaf(v) => g(v)
    case Branch(l,r) => f(fold(l,g)(f),fold(r,g)(f))
  }

  def map[A,B](t:Tree[A])(f:A=>B): Tree[B] = fold(t)((l,r)=>Branch(l,r),v=>Leaf(f(v)))

}
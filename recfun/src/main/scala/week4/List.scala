package week4

/**
  * Created by wborkows on 10/26/2016.
  */
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int, list: List[T]): T
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false

  def nth(n: Int, xs: List[T]): T = {
      if (xs.isEmpty) throw new IndexOutOfBoundsException("Cons.nth")
      else if (n == 0) xs.head
      else nth(n-1, xs.tail)
  }
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  def nth(n: Int, list: List[T]) = throw new IndexOutOfBoundsException("Nil.nth")
}

object List {
  def isort(xs: List[Int]): List[Int] = xs match{
    case List() => new Nil[Int]
    case y :: ys => insert(y, isort(ys))

  }

  def insert[Int](x: Int, xs: List[Int]): List[Int] = xs[Int] match {
    case List() => List(x)
    case y :: ys => if (x < y) x :: xs[Int] else y :: insert(x, ys)
  }
  // List()
  def apply[T] = new Nil[T]
  // List(1)
  def apply[T](x: T): List[T] = new Cons[T](x, new Nil[T])
  // List(1, 2) = List.apppy(1, 2)
  def apply[T](x1: T, x2: T): List[T] = new Cons[T](x1, new Cons[T](x2, new Nil[T]))


}
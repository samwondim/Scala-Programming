package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail(ints: List[Int]): List[Int] = ints match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def apply[A](as: A*): List[A] = //variadic function syntax
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  @main def main(args: Array[String]): Unit = {
    val l1: List[Int] = List(1,2,3,4)
    val l2: List[Int] = tail(l1)
    println(l2)  
  }
}

import org.scalatest._

class BookCh3 extends FlatSpec with Matchers {

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
      case Cons(d, dbs) => d * product(dbs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(h, t) => t
      }
    }

    @annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] = {
      n match {
        case 0 => l
        case x if x > 0 => drop(tail(l), x - 1)
      }
    }

    @annotation.tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      l match {
        case Nil => l
        case Cons(h, t) => {
          if (!f(h)) {
            l
          } else {
            dropWhile(t, f)
          }
        }
      }
    }

    def init[A](l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(h, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }
    }

    def setHead[A](el: A, l: List[A]) = {
      l match {
        case Nil => List(el)
        case Cons(h, t) => Cons(el, t)
      }
    }

    def foldRight[A,B](as:List[A], z:B)(f:(A,B) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
    }

    def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
  }


  "Ex3.1" should "implement a recursive fibonacci function with tail recursion" in {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    x should be (3)
  }

  "Ex3.2" should "implement a tail function" in {
    List.tail(List(1,2,3)) should be (List(2,3))
    List.tail(List(1)) should be (Nil)
    List.tail(Nil) should be (Nil)
  }

  "Ex3.3" should "implement a setHead function that replaces head with another element" in {
    List.setHead(1, List(2,2,3)) should be (List(1,2,3))
    List.setHead(1, List(2)) should be (List(1))
    List.setHead(1, Nil) should be (List(1))
  }

  "Ex3.4" should "implement a drop function that drops n first elements of the list" in {
    List.drop(List(1,2,3), 0) should be (List(1,2,3))
    List.drop(List(1,2,3), 1) should be (List(2,3))
    List.drop(List(1,2,3), 2) should be (List(3))
    List.drop(List(1,2,3), 3) should be (Nil)
    List.drop(List(1,2,3), 4) should be (Nil)
  }

  "Ex3.5" should "implement a dropWhile function that drops elements until condition is fulfilled" in {
    List.dropWhile(List(1,3,5,6), (x: Int) => {x % 2 != 0}) should be (List(6))
  }

  "Ex3.6" should "implement init function that returns a list from an input list without the last element" in {
    List.init(List(1,2,3)) should be (List(1,2))
    List.init(List(1)) should be (Nil)
    List.init(Nil) should be (Nil)
  }

  "Ex3.7" should "consider the ways to short circuit the foldRight recursion" in {
    val lut = List(1,2,3,4,5)
    val ccCondition = (a: Int) => a > 3
    def takeWhile[A](l: List[A], cond: A => Boolean): List[A] = {
      l match {
        case Nil => Nil
        case Cons(h, _) if cond(h) => Nil
        case Cons(h, t) => Cons(h, takeWhile(t, cond))
      }
    }
    
    takeWhile(lut, ccCondition) should be (List(1,2,3))
    List.foldRight(takeWhile(lut, ccCondition), 0)(_ + _) should be (6)

    def foldRightWhile[A,B](as:List[A], z:B, cond: A => Boolean)(f:(A,B) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => {
          if(!cond(x))
            f(x, foldRightWhile(xs, z, cond)(f))
          else
            z
        }
      }
    }

    foldRightWhile(lut, 0, ccCondition)(_ + _) should be (6)
  }
  
}
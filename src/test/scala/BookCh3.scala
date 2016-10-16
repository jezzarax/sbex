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

  "Ex2.3" should "implement a tail function" in {

    List.tail(List(1,2,3)) should be (List(2,3))
    List.tail(List(1)) should be (Nil)
    List.tail(Nil) should be (Nil)

  }
  
}
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

    def foldRight[A,B](as:List[A], z:B)(f:(A, B) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
    }

    @annotation.tailrec
    def foldLeft[A,B](as: List[A], z:B)(f:(B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(x,xs) => foldLeft(xs, f(z,x))(f)
      } 
    }

    def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

    def length[A](l: List[A]) = l match {
      case Nil => 0
      case _ => foldRight(l, 0)((i,acc) => acc + 1)
    }

    def reverse[A](l: List[A]): List[A] = List.foldLeft(l, Nil:List[A])((acc, el) => Cons(el,acc)) 

    def map[A,B](as: List[A])(f: A=> B): List[B] = {
      List.foldLeft(List.reverse(as), Nil: List[B])((acc:List[B], el: A) => Cons(f(el), acc))
    }

    def filter[A](as: List[A])(f: A=> Boolean): List[A] = {
      List.foldLeft(List.reverse(as), Nil: List[A])((acc:List[A], el: A) => {
          el match {
            case el if f(el) => Cons(el, acc)
            case _ => acc
          }
        }
      )
    }

    def flatMap[A,B](as: List[A])(f:A => List[B]): List[B] = {
      List.foldRight(as, Nil:List[B])((el, acc) => {
        List.foldRight(f(el), acc)((el:B, acc:List[B]) => Cons(el, acc))
        }
      )
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

  "Ex3.8" should "check what happens when combining foldRight with Cons" in {
    List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) should be (List(1,2,3))
  }

  "Ex3.9" should "implement a length function using foldRight" in {
    List.length(List(1,2,3)) should be (3)
    List.length(List(1)) should be (1)
    List.length(Nil) should be (0)
  }

  "Ex3.10" should "implement foldLeft with tail recursion" in {
    List.foldLeft(List(1,2,3), 0)(_ + _) should be (6)
  }

  "Ex3.11" should "implement sum, product and length using foldLeft" in {
    def sum(l: List[Int]) = List.foldLeft(l, 0)(_+_)

    sum(List(1,2,3)) should be (6)

    def product(l: List[Int]) = List.foldLeft(l, 1)(_*_)

    product(List(1,2,3)) should be (6)

    def length[A](l: List[A]): Int = List.foldLeft(l, 0)((acc, el) => acc + 1)

    length(List(1,2,3)) should be (3)
  }

  "Ex3.12" should "implement reverse using a fold" in {
    def reverse[A](l: List[A]): List[A] = List.foldLeft(l, Nil:List[A])((acc, el) => Cons(el,acc)) 

    reverse(List(1,2,3)) should be (List(3,2,1))
  }

  "Ex3.13" should "implement foldLeft through foldRight and vice verca" in {
    def foldRightViaFoldLeft[A,B](as:List[A], z:B)(f:(A,B) => B): B = {
      val reversed = List.foldLeft(as, Nil:List[A])((acc, el) => Cons(el,acc))
      List.foldLeft(reversed, z)((acc, el) => f(el,acc)) 
    }

    foldRightViaFoldLeft(List(1,2,3), Nil:List[Int])(Cons(_,_)) should be (List(1,2,3))

    def foldLeftViaRight[A,B](as:List[A], z:B)(f:(A, B) => B): B = {
      List.foldRight(as, (b:B) => b)((a,g) => b => g(f(a,b)))(z)
    }

    foldLeftViaRight(List(1,2,3), 0)(_ + _) should be (6)
  }

  "Ex3.14" should "implement appending of one list to another" in {
    def append[A](l1: List[A], l2: List[A]): List[A] = {
      List.foldRight(l1, l2)(Cons(_,_))
    }

    append(List(1,2,3), List(4,5,6)) should be (List(1,2,3,4,5,6))
  }

  "Ex3.15" should "flatten multiple lists into one" in {
    def selectMany[A](as: List[List[A]]): List[A] = {
      List.foldRight(as, Nil:List[A])((el, acc) => {
        List.foldRight(el, acc)((el:A, acc:List[A]) => Cons(el, acc))
        }
      )
    }

    selectMany(List(List(1,2), List(3,4), List(4,5))) should be (List(1,2,3,4,4,5))
  }

  "Ex3.16" should "add 1 to each element of the list" in {
    def addOne(as: List[Int]): List[Int] = {
      List.foldRight(as, Nil:List[Int])((el:Int, acc:List[Int]) => Cons(el + 1, acc))
    }

    addOne(List(1,2,3)) should be (List(2,3,4))
  }

  "Ex3.17" should "produce list of strings from list of Doubles" in {
    def stringify(as: List[Double]): List[String] = {
      List.foldRight(as, Nil:List[String])((el:Double, acc:List[String]) => Cons(el.toString, acc))
    }

    stringify(List(1.0,2.0,3.0)) should be (List("1.0", "2.0", "3.0"))
  }

  "Ex3.18" should "implement map function" in {
    List.map(List(1,2,3))(_+1) should be (List(2,3,4)) 
  }

  "Ex3.19" should "implement filter function" in {
    List.filter(List(1,2,3,4))(_%2==0) should be (List(2,4))  
  }

  "Ex3.20" should "implement flatMap function" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
  }

}
import collection.mutable.Stack
import org.scalatest._

class BookCh2 extends FlatSpec with Matchers {

  "Ex2.1" should "implement a recursive fibonacci function with tail recursion" in {
    
    def fib(n: Int): Int = {

      @annotation.tailrec
      def fib_int(idx: Int, a: Int, b: Int): Int = {
        idx match {
          case 0 => a
          case _ => fib_int(idx - 1, b, a + b)
        }
      }
      fib_int(n, 0, 1)
    }

    fib(0) should be (0)
    fib(1) should be (1)
    fib(3) should be (2)
    fib(5) should be (5)
  }

  "Ex2.2" should "implement an isSorted function that checks if array is sorted according to comparison function" in {
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      @annotation.tailrec
      def isSortedFrom(n: Int): Boolean = {
        if (n>(as.length - 2)) true
        else if (!ordered(as(n), as(n+1))) false
        else isSortedFrom(n+1)
      }
      isSortedFrom(0)
    }

    isSorted(Array(1,2,3), (a:Int,b:Int) => {a < b}) should be (true)
    isSorted(Array(1,2), (a:Int,b:Int) => {a < b}) should be (true)
    isSorted(Array(2,1), (a:Int,b:Int) => {a < b}) should be (false)
    isSorted(Array(2,1,3), (a:Int,b:Int) => {a < b}) should be (false)
    isSorted(Array(1,3,2), (a:Int,b:Int) => {a < b}) should be (false)
  }

  "Ex2.3" should "implement a currying function" in {
    def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
      (a:A) => ((b:B) => f(a, b))
    }

    curry((a:Int, b:Int) => (a+b))(2)(3) should be (5)
  }

  "Ex2.4" should "implement a uncurrying function" in {
    def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
      (a: A, b: B) => f(a)(b)
    }

    def f: Int => Int => Int = a => b => a + b

    uncurry(f)(2, 3) should be (5)
  }

  "Ex2.5" should "implement a function composing two other functions" in {
    def compose[A,B,C](f: B => C, g: A => B): A => C = {
      (a:A) => f(g(a))
    }
    def addOne(a: Int) = a + 1
    def multiplyByTwo(a: Int) = a * 2

    compose(addOne, multiplyByTwo)(1) should be (3)

  }
  
}
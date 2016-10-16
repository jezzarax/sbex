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
  
}
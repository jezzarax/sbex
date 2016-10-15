import collection.mutable.Stack
import org.scalatest._

class BookCh2 extends FlatSpec with Matchers {

  "Ex2.1" should "implement a recursive fibonaci function" in {
    
    def fib(n: Int): Int = {
      n match {
        case 0 => 0
        case 1 => 1
        case _ => fib(n-2) + fib(n-1)
      }
    }

    fib(0) should be (0)
    fib(1) should be (1)
    fib(3) should be (2)
    fib(5) should be (5)
  }
  
}
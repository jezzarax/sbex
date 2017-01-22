import org.scalatest._

class BookCh7 extends FlatSpec with Matchers {

	class Par[A] {}

	object Par {
		def unit[A](a: A): Par[A] = ???
		def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
		def fork[A](a: => Par[A]): Par[A] = ???
		def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
		def run[A](a: Par[A]): A = ???

	}

	"Ex7.1" should "define a signature for map2" in {
		def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
	}



}
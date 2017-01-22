import org.scalatest._

class BookCh7 extends FlatSpec with Matchers {

	class Par[A] {}

	"Ex7.1" should "define a signature for map2" in {
		def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
	}



}
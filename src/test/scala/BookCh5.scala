import org.scalatest._

class BookCh5 extends FlatSpec with Matchers {
	
	sealed trait Stream[+A] {
		def headOption: Option[A] = this match {
	 		case Empty => None
	 		case Cons(h, t) => Some(h())
	 	}

	 	def toList: List[A] = {
	 		this match {
	 			case Empty => Nil
	 			case Cons(h, t) => h() :: t().toList
	 		}
	 	}

	 	def take(n: Int): Stream[A] = {
	 		this match {
	 			case Cons(h, t) => if (n == 0) Empty else Cons(h, () => t().take(n - 1))
	 			case Empty => Empty
	 		}
	 	}

	 	def drop(n: Int): Stream[A] = {
	 		this match {
	 			case Cons(h, t) => if (n == 0) this else t().drop(n - 1)
	 			case Empty => Empty
	 		}
	 	}
	}

	case object Empty extends Stream[Nothing]
	case class Cons[+A](h: () => A, t: () => Stream [A]) extends Stream[A]

	object Stream {
		def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
			lazy val head = hd
			lazy val tail = tl
			Cons(() => head, () => tail)
		}

		def empty[A]: Stream[A] = Empty

		def apply[A](as: A*): Stream[A] =
			if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

	}

	"Ex5.1" should "implement toList function that produces list from a stream" in {
		Stream(1,2,3,4,5).toList should be (List(1,2,3,4,5))
		Stream().toList should be (Nil)

	}

	"Ex5.2" should "implement take(n) and drop(n) functions" in {
		Stream(1,2,3,4).take(2).toList should be (List(1,2))
		Stream(1,2).take(5).toList should be (List(1,2))

		Stream(1,2,3,4).drop(2).toList should be (List(3,4))
		Stream(1,2,3,4).drop(5).toList should be (Nil)
	}
}

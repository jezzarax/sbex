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
	 	def takeWhile(p: A => Boolean): Stream[A] = {
	 		this match {
	 			case Cons(h, t) => if (!p(h())) Empty else Cons(h, () => t().takeWhile(p))
	 			case Empty => Empty
	 		}
	 	}

	 	def forAll(p: A => Boolean): Boolean = {
	 		this match {
	 			case Cons(h, t) => if (!p(h())) false else t().forAll(p)
	 			case Empty => true
	 		}
	 	}

	 	def foldRight[B](z: => B) (f: (A, => B) => B): B = {
	 		this match {
	 			case Cons(h, t) =>  f(h(), t().foldRight(z)(f))
	 			case _ => z
	 		}
	 	}

	 	def takeWhileFR(p: A => Boolean): Stream[A] = {
	 		this.foldRight(Empty: Stream[A])((el, acc) => if (!p(el)) Empty else Cons(() => el, () => acc))
	 	}

	 	def headOptionFR: Option[A] = 
	 		this.foldRight(None:Option[A])((el, acc) => Some(el))

	 	def map[B](f: A => B): Stream[B] = 
	 		this.foldRight(Empty: Stream[B])((el, acc) => Cons(() => f(el), () => acc))
	 	
	 	def filter(f: A => Boolean): Stream[A] = 
	 		this.foldRight(Empty: Stream[A])((el, acc) => if(f(el)) Cons(() => el, () => acc) else acc)
	 	def append[B>:A](s: => Stream[B]): Stream[B] = 
	 		this.foldRight(s)((el, acc) => Cons(() => el, () => acc))
	 	def flatMap[B](f: A => Stream[B]): Stream[B] = 
	 		this.foldRight(Empty: Stream[B])((el, acc) => {
	 			f(el).append(acc)
	 		})
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

	val ones: Stream[Int] = Stream.cons(1, ones)

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

	"Ex5.3" should "implement takeWhile" in {
		Stream(1,2,7,3).takeWhile(_<5).toList should be (List(1,2))
		Stream(1,2).takeWhile(_>5).toList should be (Nil)
	}

	"Ex5.4" should "implement forAll" in {
		Stream(1,2,7,3).forAll(_<5) should be (false)
		Stream(1,2,3).forAll(_<5) should be (true)

		ones.forAll(_ == 0) should be (false)
	}

	"Ex5.5" should "implement takeWhile using foldRight" in {
		Stream(1,2,7,3).takeWhileFR(_<5).toList should be (List(1,2))
		Stream(1,2).takeWhileFR(_>5).toList should be (Nil)
	}

	"Ex5.6" should "implement headOption using foldRight" in {
		Stream(1,2,3).headOptionFR should be (Some(1))
		Stream().headOptionFR should be (None)
	}

	"Ex5.7" should "implement map, filter, append and flatMap using foldRight" in {

		Stream(1,2,3).map(_+2).toList should be (List(3,4,5))
		Stream(1,2,3).filter(_%2 != 0).toList should be (List(1,3))
		Stream(1,2).append(Stream(3,4)).toList should be (List(1,2,3,4))
		Stream(1,2).flatMap(e => Stream(e, e+1)).toList should be (List(1,2,2,3))

	}
}

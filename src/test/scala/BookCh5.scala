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

	 	def mapUF[B](f: A => B): Stream[B] = Stream.unfold(this)((s) => s match {
	 		case Cons(h, t) => Some((f(h()), t()))
	 		case _ => None
	 	})

	 	def takeUF(n: Int): Stream[A] = Stream.unfold((n, this))
	 	{
	 		case (1, Cons(h, t)) => Some((h(), (0, Empty)))
	 		case (i, Cons(h, t)) if i > 1 => Some((h(), ((i-1), t())))
	 		case _ => None
	 	}

	 	def takeWhileUF(p: A => Boolean): Stream[A] = Stream.unfold(this){
	 		case Cons(h, t) if p(h()) => Some((h(), t()))
	 		case _ => None
	 	}

	 	def zipWith[B>:A, C](l2: Stream[B])(f:(A, B) => C): Stream[C] = Stream.unfold((this, l2)) {
	 		case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
	 		case (Empty, Cons(h2, t2)) => None
	 		case (Cons(h1, t1), Empty) => None
	 		case _ => None
	 	}

	 	def zipAll[B](l2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, l2)) {
	 		case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
	 		case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
	 		case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
	 		case _ => None
	 	}

	 	def startsWith[B>:A](s: Stream[B]): Boolean = {
	 		def startsWithInt[B>:A](hay: Stream[A], needle: Stream[B]): Boolean = (hay, needle) match { 
	 			case (Cons(h1, t1), Cons(h2, t2)) => (h1() == h2() && startsWithInt(t1(), t2()))
	 			case (Cons(h1, t1), Empty) => true
	 			case (Empty, Cons(h2, t2)) => false
	 			case (Empty, Empty) => true

	 		}

	 		startsWithInt(this, s)
	 	}

	 	def tails: Stream[Stream[A]] = Stream.unfold(this){ 
	 		s => s match {
	 			case Cons(h, t) => Some((s, t()))
	 			case _ => None
	 		}
		}

		def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
		    	this.foldRight((z, Stream(z)))((a, p0) => {
		    		      lazy val p1 = p0
		    		      val b2 = f(a, p1._1)
		    		      (b2, Cons(() => b2, () => p1._2))
		    		    })._2
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

		def constant[A](a: A): Stream[A] =
			Stream.cons(a, constant(a))

		def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
		def fibs: Stream[Int] = {
			def fibsInt(a: Int, b: Int): Stream[Int] = Stream.cons(a, fibsInt(b, a + b))

			fibsInt(0, 1)
		}

		def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
			f(z) match {
				case Some((a,s)) => Stream.cons(a, unfold(s)(f))
				case None => Stream.empty
			}

		def ones: Stream[Int] = Stream.cons(1, ones)

		def onesUF: Stream[Int] = Stream.unfold(1)(s => Some(1,1))
		def constantUF[A](a: A): Stream[A] = Stream.unfold(a)(s => Some(a,a))
		def fibsUF: Stream[Int] = Stream.unfold((0,1))(s => Some((s._1, (s._2, s._1 + s._2))))
		def fromUF(n: Int): Stream[Int] = Stream.unfold(n)(s => Some(s, s + 1))

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

	"Ex5.3" should "implement takeWhile" in {
		Stream(1,2,7,3).takeWhile(_<5).toList should be (List(1,2))
		Stream(1,2).takeWhile(_>5).toList should be (Nil)
	}

	"Ex5.4" should "implement forAll" in {
		Stream(1,2,7,3).forAll(_<5) should be (false)
		Stream(1,2,3).forAll(_<5) should be (true)

		Stream.ones.forAll(_ == 0) should be (false)
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

	"Ex5.8" should "implement function constant producing an infinite stream of input parameter instances" in {
		Stream.constant(2).take(3).toList should be (List(2,2,2))
		Stream.constant(2).take(5).toList should be (List(2,2,2,2,2))
	}

	"Ex5.9" should "implement a function producing an infinite stream of monotonically increasing integers starting from n" in {
		Stream.from(3).take(3).toList should be (List(3,4,5))
		Stream.from(2).take(5).toList should be (List(2,3,4,5,6))
	}

	"Ex5.10" should "implement fibonacci stream generation" in {
		Stream.fibs.take(5).toList should be (List(0,1,1,2,3))
		Stream.fibs.take(7).toList should be (List(0,1,1,2,3,5,8))
	}

	"Ex5.11" should "implement unfold" in {
		Stream.unfold(0)(a => if (a < 10) Option((a + 2, a + 2)) else None).toList should be (List(2,4,6,8,10))
	}

	"Ex5.12" should "implement fibs, from, constant and ones using unfold" in {
		Stream.constantUF(2).take(3).toList should be (List(2,2,2))
		Stream.constantUF(2).take(5).toList should be (List(2,2,2,2,2))
		Stream.fromUF(3).take(3).toList should be (List(3,4,5))
		Stream.fromUF(2).take(5).toList should be (List(2,3,4,5,6))
		Stream.fibsUF.take(5).toList should be (List(0,1,1,2,3))
		Stream.fibsUF.take(7).toList should be (List(0,1,1,2,3,5,8))
		Stream.onesUF.take(5).toList should be (List(1,1,1,1,1))
		Stream.onesUF.take(7).toList should be (List(1,1,1,1,1,1,1))
	}

	"Ex5.13" should "implement map, take, takeWhile, zipWith and zipAll using unfold" in {
		Stream(1,2,3).mapUF(_+2).toList should be (List(3,4,5))
		Stream(1,2,3,4).takeUF(2).toList should be (List(1,2))
		Stream(1,2).takeUF(5).toList should be (List(1,2))
		Stream(1,2,7,3).takeWhileUF(_<5).toList should be (List(1,2))
		Stream(1,2).takeWhileUF(_>5).toList should be (Nil)

		Stream(1,2,3).zipWith(Stream(2,3,4))((a:Int,b:Int) => (a,b)).toList should be (List((1,2),(2,3),(3,4)))
		Stream(1,2,3).zipAll(Stream(2,3,4)).toList should be 
			(List((Option(1), Option(2)), (Option(2), Option(3)), (Option(3), Option(4))))

		Stream(1,2).zipAll(Stream(2,3,4)).toList should be 
			(List((Option(1), Option(2)), (Option(2), Option(3)), (None, Option(4))))

		Stream(1,2,3).zipAll(Stream(2,3)).toList should be 
			(List((Option(1), Option(2)), (Option(2), Option(3)), (Option(3), None)))

	}

	"Ex5.14" should "implement startsWith function" in {
		Stream(1,2,3).startsWith(Stream(1,2)) should be (true)
		Stream(1,2,3).startsWith(Stream(1,3)) should be (false)
	}


	"Ex5.15" should "implement tails that returns all suffixes of the stream" in {
		Stream(1,2,3).tails.flatMap(x => x).toList should be (List(1,2,3,2,3,3))
	}

	"Ex5.16" should "implement scanRight function" in {
		Stream(1,2,3).scanRight(1)((a, b) => a*b).toList should be (List(6, 6, 3, 1))
	}


}

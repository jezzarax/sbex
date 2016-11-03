import org.scalatest._

class BookCh4 extends FlatSpec with Matchers {
	sealed trait Option[+A] {
		def map[B](f: A => B): Option[B] = {
			this match {
				case Some(v) => Some(f(v))
				case None => None
			}
		}
		def flatMap[B](f: A => Option[B]): Option[B] = {
			this match {
				case Some(v) => f(v)
				case None => None
			}
		}
		def getOrElse[B >: A](default: => B): B = {
			this match {
				case Some(v) => v
				case None => default
			}
		}
		def orElse[B >: A](ob: => Option[B]): Option[B] = {
			this match {
				case Some(v) => Some(v)
				case None => ob
			}
		}
		def filter(f: A => Boolean): Option[A] = {
			this match {
				case Some(v) if f(v) => this
				case _ => None
			}
		}
	}

	case class Some[+A](get: A) extends Option[A]
	case object None extends Option[Nothing]

	def Try[A](a: => A): Option[A] = 
		try Some(a)
		catch { case e: Exception => None }

	"Ex4.1" should "implement map, flatMap, getOrElse, orElse and filter methods" in {
		val a:Option[String] = Some("String")
		val b:Option[String] = None

		a.map(_.length) should be (Some(6))
		b.map(_.length) should be (None)

		a.flatMap((s: String) => Some(s.length)) should be (Some(6))
		a.flatMap((s: String) => None) should be (None)
		b.flatMap((s: String) => Some(s.length)) should be (None)
		b.flatMap((s: String) => None) should be (None)

		a.getOrElse("Empty") should be ("String")
		b.getOrElse("Empty") should be ("Empty")

		a.orElse(Some("Empty")) should be (Some("String"))
		b.orElse(Some("Empty")) should be (Some("Empty"))

		a.filter(_.length == 6) should be (Some("String"))
		a.filter(_.length == 5) should be (None)
		b.filter(_.length == 6) should be (None)
		b.filter(_.length == 5) should be (None)
	}

	"Ex4.2" should "implement a variance function" in {
		def variance(xs: Seq[Double]): Option[Double] = {
			def mean(xs: Seq[Double]): Option[Double] = {
				xs.length match {
					case 0 => None
					case _ => Some(xs.sum/xs.length)
				}
			}
			mean(xs).flatMap(m => mean(xs.map(x => { math.pow(x - m, 2) })))
		}

		variance(List(1.0, 2.0, 3.0, 4.0)) should be (Some(1.25))
		variance(List(0.0)) should be (Some(0.0))
		variance(List[Double]()) should be (None)
	}

	"Ex4.3" should "implement map2 function operating on two Option instances" in {

		def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
			a.flatMap(ai => b.map(bi => f(ai, bi)))
		}

		map2(Some(1), Some("2"))(_.toString + _) should be (Some("12"))
		map2(None, Some("2"))(_.toString + _) should be (None)
		map2(Some(1), None)(_.toString + _) should be (None)
		map2(None, None)(_.toString + _) should be (None)
	}

	"Ex4.4" should "implement sequence function combining List of Options into an Option from List" in {
		def sequence[A](a: List[Option[A]]): Option[List[A]] = {
			a match {
				case h :: t => h.flatMap(hi => sequence(t).map( hi :: _ ))
				case Nil => Some(Nil)
			}
		}

		sequence(List(Some(1), Some(2))) should be (Some(List(1,2)))
		sequence(List(Some(1), None, Some(2))) should be (None)
		sequence(Nil) should be (Some(Nil))
	}

	"Ex4.5" should "implement traverse function without using map/sequence combination" in {
		def traverse[A,B](a: List[A])(f:A => Option[B]): Option[List[B]] = {
			a match {
				case h :: t => f(h).flatMap(hi => traverse(t)(f).map( hi :: _ ))
				case Nil => Some(Nil)
			}
		}

		
		traverse(List("1", "2"))(i => Try(i.toInt)) should be (Some(List(1,2)))
		traverse(List("1", "a"))(i => Try(i.toInt)) should be (None)
		traverse[String, Int](Nil)(i => Try(i.toInt)) should be (Some(Nil))

	}

	sealed trait Either[+E, +A] {
		def map[B](f: A => B): Either[E, B] = {
			this match {
				case Right(v) => Right(f(v))
				case Left(v) => Left(v)
			}
		}
		def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
			this match {
				case Right(v) => f(v)
				case Left(v) => Left(v)
			}
		}
		def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
			this match {
				case Right(v) => Right(v)
				case _ => b
			}
		}
		def map2[EE >: E, B, C](b: => Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
			this.flatMap(ai => b.map(bi => f(ai,bi)))
		}
	}

	case class Left[+E](value: E) extends Either[E, Nothing]
	case class Right[+A](value: A) extends Either[Nothing, A]

	"Ex4.6" should "implement map, flatMap, orElse and map2 methods for Either monad" in {
		val a:Either[String, Int] = Left("String")
		val b:Either[String, Int] = Right(5)

		a.map(_.toString) should be (Left("String"))
		b.map(_.toString) should be (Right("5"))

		a.flatMap(v => Right(v.toString)) should be (Left("String"))
		b.flatMap(v => Right(v.toString)) should be (Right("5"))

		a.orElse(Right("Something is not right")) should be (Right("Something is not right"))
		b.orElse(Left("whatever")) should be (Right(5))

		a.map(_.toString) should be (Left("String"))
		b.map(_.toString) should be (Right("5"))

		a.map2(Left(" is left"))(_+_) should be (Left("String"))
		a.map2(Right(10))(_+_) should be (Left("String"))

		b.map2(Left(" is left"))(_+_) should be (Left(" is left"))
		b.map2(Right(10))(_+_) should be (Right(15))

	}

	"Ex4.7" should "implement sequence and traverse functions for Either" in {
		def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
			es match {
				case h :: t => h.flatMap(hi => sequence(t).map( hi :: _ ))
				case Nil => Right(Nil)
			}
		}

		def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
			as match {
				case h :: t => f(h).flatMap(hi => traverse(t)(f).map( hi :: _ ))
				case Nil => Right(Nil)
			}
		}

		sequence(List(Right(1), Right(2))) should be (Right(List(1,2)))
		sequence(List(Right(1), Left("not right"), Right(2))) should be (Left("not right"))
		sequence(List(Right(1), Left("not right"), Left("totally not right"))) should be (Left("not right"))
		sequence(Nil) should be (Right(Nil))		

		def tryToParse(a: String): Either[String, Int] = {
			Try(a.toInt) match {
				case Some(n) => Right(n)
				case _ => Left(a)
			}
		}


		traverse(List("1", "2"))(tryToParse) should be (Right(List(1,2)))
		traverse(List("1", "a"))(tryToParse) should be (Left("a"))
		traverse[String, String, Int](Nil)(tryToParse) should be (Right(Nil))

	}

}

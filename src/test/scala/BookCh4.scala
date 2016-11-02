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

}

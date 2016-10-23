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
}
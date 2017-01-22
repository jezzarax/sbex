import scala.concurrent.duration.TimeUnit

import org.scalatest._

class BookCh7 extends FlatSpec with Matchers {

	trait Callable[A] { def call: A }

	trait Future[A] {
		def get: A
		def get(timeout: Long, unit: TimeUnit): A
		def cancel(evenIfRunning: Boolean): Boolean
		def isDone: Boolean
		def isCancelled: Boolean
	}

	class ExecutorService {
		def submit[A](a: Callable[A]): Future[A] = ???
	}


	type Par[A] = ExecutorService => Future[A]

	object Par {
		def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

		private case class UnitFuture[A](get: A) extends Future[A] {
			def isDone = true
			def get(timeout: Long, units: TimeUnit) = get
			def isCancelled = false
			def cancel(evenIfRunning: Boolean): Boolean = false
		}

		def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = 
			(es: ExecutorService) => {
				val af = a(es)
				val bf = b(es)
				UnitFuture(f(af.get, bf.get))
			}

		def fork[A](a: => Par[A]): Par[A] = 
			(es: ExecutorService) => {
				es.submit(new Callable[A]{
					def call = a(es).get
				})
			}
		def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
		def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

	}


	"Ex7.1" should "define a signature for map2" in {
		def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
	}



}
import org.scalatest._

class BookCh6 extends FlatSpec with Matchers {
	

	trait RNG {
		def nextInt: (Int, RNG)
	}

	case class SimpleRNG(seed: Long) extends RNG{
		def nextInt: (Int, SimpleRNG) = {
			val newSeed = (seed * 0x5DEECEE66DL + 0xBL) & 0xFFFFFFFFFFFFL
			val nextRNG = SimpleRNG(newSeed)
			val n = (newSeed >>> 16).toInt
			(n, nextRNG)
		}
	}

	case class RNGMock(seed: Int) extends RNG {
		def nextInt: (Int, RNGMock) = (seed, RNGMock(seed))
	}

	case class RNGMockWithIncrement(seed: Int) extends RNG {
		def nextInt: (Int, RNGMockWithIncrement) = (seed, RNGMockWithIncrement(seed + 1))
	}

	def nonNegativeInt(rng: RNG): (Int, RNG) = 
		rng.nextInt match {
			case (a, r) if a == Int.MinValue => (0, r)
			case (a, r) => (math.abs(a), r)
		}

	def double(rng: RNG):(Double, RNG) = 
		rng.nextInt match {
			case (a, r) if a == 0 => (0.0, r)
			case (a, r) => (math.abs(1.0/a), r)
		}

	def intDouble(rng: RNG): ((Int, Double), RNG) = {
		val r1 = nonNegativeInt(rng)
		val r2 = double(r1._2)
		((r1._1, r2._1), r2._2)
	}

	def doubleInt(rng: RNG): ((Double, Int), RNG) = {
		val r1 = double(rng)
		val r2 = nonNegativeInt(r1._2)
		((r1._1, r2._1), r2._2)
	}

	def double3(rng: RNG): ((Double, Double, Double), RNG) = {
		val r1 = double(rng)
		val r2 = double(r1._2)
		val r3 = double(r2._2)
		((r1._1, r2._1, r3._1), r3._2)
	}

	def ints(num: Int)(rng: RNG): (List[Int], RNG) = {
		def intsInt(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
	    	if (count == 0)
	        	(xs, r)
	    	else {
	        	val (x, r2) = r.nextInt
	        	intsInt(count - 1, r2, x :: xs)
	    	}
    	intsInt(num, rng, List())
	}


	"Ex6.1" should "implement nonNegativeInt" in {
		nonNegativeInt(RNGMock(10))._1 should be (10)
		nonNegativeInt(RNGMock(-10))._1 should be (10)
		nonNegativeInt(RNGMock(Int.MinValue))._1 should be (0)
	}

	"Ex6.2" should "implement double function that produces a random number between 0 and 1" in {
		double(RNGMock(10))._1 should be (0.1)
		double(RNGMock(-10))._1 should be (0.1)
		double(RNGMock(1))._1 should be (1)
		double(RNGMock(-1))._1 should be (1)
	}

	"Ex6.3" should "implement intDouble, doubleInt and double3 combining existing generator functions" in {
		intDouble(RNGMockWithIncrement(3))._1 should be ((3, 0.25))
		doubleInt(RNGMockWithIncrement(4))._1 should be ((0.25, 5))
		double3(RNGMockWithIncrement(3))._1 should be (((1.0/3.0), 0.25, 0.2))
	}

	"Ex6.4" should "implement ints function generating list of random integers" in {
		ints(5)(RNGMockWithIncrement(2))._1 should be (List(6,5,4,3,2))
	}


}

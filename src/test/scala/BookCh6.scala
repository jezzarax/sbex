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

	def nonNegativeInt(rng: RNG): (Int, RNG) = 
		rng.nextInt match {
			case (a, r) if a == Int.MinValue => (0, r)
			case (a, r) => (math.abs(a), r)
		}

	"Ex6.1" should "implement nonNegativeInt" in {
		case class RNGMock(seed: Int) extends RNG {
			def nextInt: (Int, RNGMock) = (seed, RNGMock(seed))
		}

		nonNegativeInt(RNGMock(10))._1 should be (10)
		nonNegativeInt(RNGMock(-10))._1 should be (10)
		nonNegativeInt(RNGMock(Int.MinValue))._1 should be (0)

	}


}

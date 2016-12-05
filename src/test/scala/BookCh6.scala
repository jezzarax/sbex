import org.scalatest._

class BookCh6 extends FlatSpec with Matchers {
    

    trait RNG {
        def nextInt: (Int, RNG)
    }

    case class SimpleRNG(seed: Long) extends RNG {
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

    case class RNGMockWithDecrement(seed: Int) extends RNG {
        def nextInt: (Int, RNGMockWithDecrement) = (seed, RNGMockWithDecrement(seed - 1))
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

    type Rand[+A] = RNG => (A, RNG)

    def int: Rand[Int] = rng => rng.nextInt

    def unit[A](a: A): Rand[A] =
        rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] = 
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }

    def doubleMap: Rand[Double] =
        map(nonNegativeInt)(i => i match {
            case n if n == 0 => 0.0
            case n => 1.0/n
        })

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = 
        rng => {
            val (a, rng1) = ra(rng)
            val (b, rng2) = rb(rng1)
            (f(a, b), rng2)
        }        

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
        rng => 
            fs.foldRight((List[A](), rng))((el, acc) => {
                acc match {
                    case (els, oldRngState) => {
                        val (a, newRngState) = el(oldRngState)
                        (a :: els, newRngState)
                    }
                }
                
            })

    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
        rng => {
            val (a, rng1) = f(rng)
            g(a)(rng1)
            
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

    "Ex6.5" should "implement double in a more elegant way using map" in {
        doubleMap(RNGMock(10))._1 should be (0.1)
        doubleMap(RNGMock(-10))._1 should be (0.1)
        doubleMap(RNGMock(1))._1 should be (1)
        doubleMap(RNGMock(-1))._1 should be (1)
    }

    "Ex6.6" should "implement map2 that combines two operation over the state" in {
        map2(double, nonNegativeInt)((a,b) => (b,a))(RNGMock(10))._1 should be ((10, 0.1))
    }

    "Ex6.7.1" should "implement a sequence function that produces a list of random values" in {
        sequence(List.fill(5)(nonNegativeInt _))(RNGMock(10))._1 should be (List(10,10,10,10,10))
        sequence(List(unit(1), unit(2), unit(3)))(RNGMock(10))._1 should be (List(1,2,3))
    }

    "Ex6.7.2" should "implement ints using sequence" in {
        def sequentialInts(num: Int)(rng: RNG): (List[Int], RNG) = {
            sequence(List.fill(num)((a: RNG) => int(a)))(rng)
        }
        sequentialInts(5)(RNGMockWithIncrement(2))._1 should be (List(6,5,4,3,2))
    }

    "Ex6.8.1" should "implement flatMap for Rand type" in {
        flatMap(int)(i => ints(i) _)(RNGMock(2))._1 should be (List(2,2))
    }

    "Ex6.8.2" should "implement nonNegativeLessThan" in {
        def nonNegativeLessThan(n: Int): Rand[Int] = 
            flatMap(nonNegativeInt)(i => {
                val mod = i % n
                if(i + (n - 1) - mod >= 0)
                    unit(mod)
                else
                    nonNegativeLessThan(mod)
            })

        nonNegativeLessThan(11)(RNGMockWithDecrement(Int.MaxValue))._1 should be (0)
    }

    "Ex6.9" should "implement map and map2 using flatMap" in {
        def mapUsingFlatMap2[A,B](s: Rand[A])(f: A => B): Rand[B] = 
            flatMap(s)(a => unit(f(a)))

        mapUsingFlatMap2(nonNegativeInt)(_ + 1)(RNGMock(10))._1 should be (11)

        def map2UsingFlatMap2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = 
            flatMap(ra)(a => map(rb)(b => f(a, b)))

        map2UsingFlatMap2(double, nonNegativeInt)((a,b) => (b,a))(RNGMock(10))._1 should be ((10, 0.1))
        

    }


}

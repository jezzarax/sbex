import org.scalatest._


class BookCh6StateMachine extends FlatSpec with Matchers {


  case class IntState[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): IntState[S, B] = flatMap(a => IntState.unit(f(a)))

    def map2[B, C](sb: IntState[S, B])(f: (A, B) => C): IntState[S, C] = flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => IntState[S, B]): IntState[S, B] = IntState(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })


  }

  object IntState {

    def modify[S](f: S => S): IntState[S, Unit] = get.flatMap { s => set(f(s)).map(_ => ()) }

    def get[S]: IntState[S, S] = IntState(s => (s, s))

    def set[S](s: S): IntState[S, Unit] = IntState(_ => ((), s))

    def unit[S, A](a: A): IntState[S, A] =
      IntState(s => (a, s))

    def sequence[S, A](sas: List[IntState[S, A]]): IntState[S, List[A]] = {
      def go(s: S, actions: List[IntState[S, A]], acc: List[A]): (List[A], S) =
        actions match {
          case Nil => (acc.reverse, s)
          case h :: t => h.run(s) match {
            case (a, s2) => go(s2, t, a :: acc)
          }
        }

      IntState((s: S) => go(s, sas, List()))
    }
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  "Ex6.11" should "implement a Machine with state" in {
    object Candy {
      def update = (i: Input) => (s: Machine) =>
        (i, s) match {
          case (_, Machine(_, 0, _)) => s
          case (Coin, Machine(false, _, _)) => s
          case (Turn, Machine(true, _, _)) => s
          case (Coin, Machine(true, candy, coin)) =>
            Machine(false, candy, coin + 1)
          case (Turn, Machine(false, candy, coin)) =>
            Machine(true, candy - 1, coin)
        }

      def simulateMachine(inputs: List[Input]): IntState[Machine, (Int, Int)] =
        IntState.sequence {
          val compose: (Input) => IntState[Machine, Unit] = (IntState.modify[Machine] _).compose(update)
          inputs map compose
        }
          .flatMap(_ => IntState.get.map(s => (s.coins, s.candies)))
    }
  }


}
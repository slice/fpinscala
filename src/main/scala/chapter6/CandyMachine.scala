package zone.slice.fpinscala.chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, coins: Int, candies: Int)

object Machine {
  // Exercise 6.11
  def simulate(input: Input): State[Machine, Unit] =
    State {
      case machine @ Machine(locked, coins, candies) =>
        val noop = ((), machine)
        input match {
          case Coin if locked =>
            ((), machine.copy(coins = coins + 1, locked = false))
          case Turn if locked => noop
          case Coin           => noop
          case Turn =>
            ((), machine.copy(candies = candies - 1, locked = true))
        }
    }
  def simulateInputs(inputs: List[Input]): State[Machine, (Int, Int)] =
    inputs match {
      case Nil =>
        // we're at the end now, give the (coins, candy) as the "result"
        State { machine => ((machine.coins, machine.candies), machine) }
      case h :: t =>
        simulate(h).flatMap(_ => simulateInputs(t))
    }
}

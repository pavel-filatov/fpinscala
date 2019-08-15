package ch06state

import ch06state.State._

object FiniteStateMachineApp extends FiniteStateMachineApp with App {

  val initialMachine: Machine = Machine(isLocked = true, candies = 5, coins = 10)
  // val inputs = Coin :: Turn :: Coin :: Turn :: Coin :: Turn :: Coin :: Turn :: Nil
  val inputs = Nil

  println(simulateMachine(inputs) run initialMachine)
}

class FiniteStateMachineApp {

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(isLocked: Boolean, candies: Int, coins: Int) {
    def takeCoinIntoAccount(): Machine = copy(coins = coins + 1)

    def dispense(): Machine = copy(candies = candies - 1)

    def unlock(): Machine = copy(isLocked = false)

    def lock(): Machine = copy(isLocked = true)

    def isOutOfCandies: Boolean = candies == 0
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.candies, s.coins)
    

  def update(input: Input)(machine: Machine): Machine =
    (input, machine) match {
      case (_, Machine(_, 0, _)) => machine
      case (Coin, Machine(true, candies, coins)) => 
        Machine(false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) =>
        Machine(true, candies - 1, coins)
      case _ => machine
    }

  private def processSingleInput(currentState: State[Machine, (Int, Int)],
                                 input: Input): State[Machine, (Int, Int)] =
    for {
      machine <- get[Machine]
      candies = machine.candies
      coins   = machine.coins

      current = (machine, candies, coins)

      (newMachine, updatedCandies, updatedCoins) =
        if (machine.isOutOfCandies)
          current
        else input match {
          case Coin =>
            if (machine.isLocked)
              (machine.unlock().takeCoinIntoAccount(), candies, coins + 1)
            else current
          case Turn =>
            if (machine.isLocked) current
            else
              (machine.dispense().lock(), candies - 1, coins)
        }

      _ <- set(newMachine)

    } yield (updatedCandies, updatedCoins)

}

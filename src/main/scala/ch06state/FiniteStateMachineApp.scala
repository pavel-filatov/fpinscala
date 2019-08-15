package ch06state

object FiniteStateMachineApp extends FiniteStateMachineApp with App {

  val initialMachine: Machine = Machine(isLocked = true, candies = 5, coins = 10)
  val inputs = Coin :: Turn :: Coin :: Turn :: Coin :: Turn :: Nil

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

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val initialState: State[Machine, (Int, Int)] = State.unit((0, 0))
    inputs.foldLeft(initialState)(processSingleInput)
  }

  private def processSingleInput(currentState: State[Machine, (Int, Int)],
                                 input: Input): State[Machine, (Int, Int)] =
    for {
      state <- currentState
      machine <- currentState.get

      (candies, coins) = state

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

      _ <- currentState.set(newMachine)

    } yield (updatedCandies, updatedCoins)

}

package fpscala.chapter6

/**
 * Created by sajit.kunnumkal on 11/25/2015.
 */
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def transition(in: Input): Machine = {
    in match {
      case Coin =>
        if (locked) {
          println("Unlocking machine")
        }
        else {
          println("Coin in an unlocked machine..")
        }
        Machine(false, candies, coins + 1)
      case Turn =>
        if (locked) {
          println("Turn a locked machine.nothing")
          Machine(true, candies, coins)
        }
        else {
          println("Dispensed candy")
          Machine(true, candies - 1, coins)
        }

    }
  }


}
object CandyShop {
  type State[S,+A] = S => (A,S)
  val machine = Machine(true,10,0)
  val lockedTurn: State[Machine, (Int, Int)] = { machine => ((machine.candies, machine.coins), machine) }
  val unlockedCoin: State[Machine, (Int, Int)] = { machine => (
    (machine.candies, machine.coins + 1), Machine(false, machine.candies, machine.coins + 1))
  }

  def altSimulation(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  def simulateMachine(inputs: List[Input], initState: Machine): Machine = {
    def doSimulateMachine(inputs: List[Input], machineState: Machine): Machine = {
      if (inputs.isEmpty) {
        println("End simulation ****")
        machineState
      }
      else {
        val in = inputs.head
        doSimulateMachine(inputs.tail, machineState.transition(in))
      }
    }
    println("Starting simulation *****")
    doSimulateMachine(inputs,initState)
  }


  def printState(machine:Machine) = {
    println("{Locked?:" + machine.locked+",candies:"+machine.candies+",coins:"+machine.coins+"}")
  }
}

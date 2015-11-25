package fpscala.chapter6

/**
 * Created by sajit.kunnumkal on 11/25/2015.
 */
sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked:Boolean, candies:Int, coins:Int)
object CandyShop {
  type State[S,+A] = S => (A,S)
  val machine = Machine(true,10,0)

  val initState: State[Machine, (Int, Int)] = ???

  def simulateMachine(inputs:List[Input]):State[Machine, (Int,Int)] = {
    def doSimulateMachine(inputs:List[Input],currentState:State[Machine,(Int,Int)]):State[Machine,(Int,Int)] = ???
    doSimulateMachine(inputs,initState)
  }

  def printState(machine:Machine) = {
    println("{Locked?:" + machine.locked+",candies:"+machine.candies+",coins:"+machine.coins+"}")
  }
}

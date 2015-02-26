import monocle.macros.Lenses
import monocle.Lens

import State._
import Ressource._

object Antike extends App {
  val moves: List[Move] = List(Marble, Temple, Marble, Gold, Temple)
  val initRessouce = Ressource(amount = 3, cities = 1, temples = 0)
  val initState = State(0, 0, 0, initRessouce, initRessouce, initRessouce)
  
  val states = moves.scanLeft(initState)((state, move) => move m state)
  
  println(showGame(moves, states))
  
  def showGame(moves: List[Move], states: List[State]): String = {
    moves zip states.tail map { case (m, s) => m + "\t" + showState(s) } mkString "\n"
  }

  def showState(s: State): String = { import s._
    s"""
      ${marble.amount},${iron.amount},${gold.amount}:$coins
      ${actives + idles}A
      ${marble.temples + iron.temples + gold.temples}T
      ${marble.cities + iron.cities + gold.cities}C
    """ split "\n" map (_.trim) mkString " "
  }
}

// State

@Lenses
case class State(actives: Int, idles: Int, coins: Int, marble: Ressource, iron: Ressource, gold: Ressource)

@Lenses
case class Ressource(amount: Int, cities: Int, temples: Int)


// Actions

sealed abstract class Move(val m: State => State)

abstract class Collect(lens: Lens[State, Ressource]) extends Move(
  lens.modify(rs => amount.modify(_ + rs.cities + 3 * rs.temples)(rs)) andThen coins.modify(_ + 1)
)

abstract class Spend(price: Int, ressource: Lens[State, Ressource], product: Lens[State, Int]) extends Move({s =>
  val ressources: Int = ressource composeLens amount get s
  val spending: Int = ((ressources + s.coins) / price) * price
  val spendRessources: State => State  = (ressource composeLens amount).modify(_ - (ressources min spending))
  val spendCoins: State => State  = coins.modify(_ - (0 max (spending - ressources)))
  val produce: State => State  = product.modify(_ + (ressources + s.coins) / price)
  spendRessources andThen spendCoins andThen produce apply s
})

case object Marble extends Collect(marble)
case object Iron extends Collect(iron)
case object Gold extends Collect(gold)
case object Science extends Move(identity)
case object Move extends Move(s => actives.modify(_ + s.idles) andThen idles.set(0) apply s)
case object Temple extends Spend(5, marble, marble composeLens temples)
case object Army extends Spend(2, iron, idles)

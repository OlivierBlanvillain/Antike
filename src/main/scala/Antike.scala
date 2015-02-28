import monocle.macros.Lenses
import monocle.Lens

import State._
import Ressource._
import Moves._
import WheelState._

object Antike extends App {
  val moves: List[Move] = List(
    Bellona,
    Marble, Ferrum, TempleM, Movemnt2, CityM,
    Marble, TempleM,
    Marble, Gold, CityM,
    Marble, TempleM
  ) // 11: Temple     2,0,2:2 0:2A 4T 5C

  val initRessouce = Ressource(amount = 3, cities = 1, temples = 0)
  val initState = State(0, 0, 0, initRessouce, initRessouce, initRessouce, Movemnt2WS)
  
  val states = moves.scanLeft(initState)((state, m) => m.wheelMove andThen m.action apply state)
  
  println(showGame(moves, states))
  
  def showGame(moves: List[Move], states: List[State]): String = {
    val turns = moves zip states filterNot (_._1.ws == SubActionWS) map (_._2)
    (turns ::: List(states.last)).zipWithIndex.tail map {
      case (s, i) => i + ": " + s.wheelState.toString.replace("WS", "") + "   \t" + showState(s)
    } mkString "\n"
  }

  def showState(s: State): String = {
    import s._
    s"""
      ${marble.amount},${iron.amount},${gold.amount}:$coins
      ${actives}:${idles}A
      ${marble.temples + iron.temples + gold.temples}T
      ${marble.cities + iron.cities + gold.cities}C
    """ split "\n" map (_.trim) mkString " "
  }
}

@Lenses case class State(
  actives: Int,
  idles: Int,
  coins: Int,
  marble: Ressource,
  iron: Ressource,
  gold: Ressource,
  wheelState: WheelState)

@Lenses case class Ressource(
  amount: Int,
  cities: Int,
  temples: Int)

object WheelState {
  sealed trait WheelState
  case object MarbleWS extends WheelState
  case object ScienceWS extends WheelState
  case object Movemnt1WS extends WheelState
  case object FerrumWS extends WheelState
  case object TempleWS extends WheelState
  case object GoldWS extends WheelState
  case object Movemnt2WS extends WheelState
  case object ArmyWS extends WheelState
  case object SubActionWS extends WheelState
  val wheel = List(MarbleWS, ScienceWS, Movemnt1WS, FerrumWS, TempleWS, GoldWS, Movemnt2WS, ArmyWS)
  val wheel2 = wheel ::: wheel
}

class Move(val action: State => State, val ws: WheelState) {
  val wheelMove: State => State = ws match {
    case SubActionWS => identity[State]
    case _ => s =>
      val distance = wheel2.dropWhile(_ != s.wheelState).tail.takeWhile(_ != ws).size
      val cost = 0 max (distance - 3)
      wheelState.set(ws) andThen spend(cost, iron) apply s
  }
}

object Moves {
  def collect(lens: Lens[State, Ressource]): State => State =
    lens.modify(rs => amount.modify(_ + rs.cities + 2 * rs.temples)(rs)) andThen coins.modify(_+1)
  
  def spend(n: Int, ressource: Lens[State, Ressource]): State => State = { s =>
    val ressources: Int = ressource composeLens amount get s
    val spendRessources = (ressource composeLens amount).modify(_ - (ressources min n))
    val spendCoins = coins.modify(_ - (0 max (n - ressources)))
    spendRessources andThen spendCoins apply s
  }
  
  def untilBroke(m: State => State): State => State =
    s => if(m(s).coins < 0) s else untilBroke(m)(m(s))
  
  def buildCity(ressource: Lens[State, Ressource]): State => State = {
    val buildCity = ressource composeLens cities modify (_+1)
    val spendRessources = spend(1, marble) andThen spend(1, iron) andThen spend(1, gold)
    val useArmy = actives.modify(_-1) andThen idles.modify(_+1)
    buildCity andThen spendRessources andThen useArmy
  }
  
  def buildTemples(ressource: Lens[State, Ressource]): State => State =
    untilBroke(spend(5, marble) andThen (ressource composeLens temples).modify(_+1))
  
  case object Marble extends Move(collect(marble), MarbleWS)
  case object Ferrum extends Move(collect(iron), FerrumWS)
  case object Gold extends Move(collect(gold), GoldWS)
  case object Science extends Move(identity, ScienceWS)
  case object Movemnt1 extends Move(s => actives.modify(_ + s.idles) andThen idles.set(0) apply s, Movemnt1WS)
  case object Movemnt2 extends Move(s => actives.modify(_ + s.idles) andThen idles.set(0) apply s, Movemnt2WS)
  case object TempleM extends Move(buildTemples(marble), TempleWS)
  case object TempleI extends Move(buildTemples(iron), TempleWS)
  case object TempleG extends Move(buildTemples(gold), TempleWS)
  case object Bellona extends Move(Army.action andThen idles.modify(_+1), ArmyWS)
  case object Army extends Move(untilBroke(spend(2, iron) andThen idles.modify(_+1)), ArmyWS)
  case object CityM extends Move(buildCity(marble), SubActionWS)
  case object CityI extends Move(buildCity(iron), SubActionWS)
  case object CityG extends Move(buildCity(gold), SubActionWS)
}

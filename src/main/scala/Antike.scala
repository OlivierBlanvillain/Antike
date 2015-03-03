import monocle.macros.Lenses
import monocle.Lens

import State._
import Ressource._
import Moves._
import WheelState._
import Symbols._

object Antike extends App {
  var moves: List[Move] = null


  moves = List(
    Bellona,
    Marble, Iron, TempleM, Movement2, CityM,
    Marble, TempleM, CityM,
    Marble, TempleM, Movement2, CityM,
    Marble, TempleM
  ) // 12: Temple      5,0,0:0 1:1A 4T 6C
  
  moves = List(
    Bellona,
    Marble, Iron, TempleM, Movement2, CityM,
    Marble, TempleM
    //  CityM,
    // Marble, TempleM, Movement2, CityM,
    // Marble, TempleM
  ) // 12: Temple      5,0,0:0 1:1A 4T 6C

  moves = List(
    Bellona,
    Marble,
    Iron,
    TempleM,
    Movement2, CityM,
    Marble,
    TempleM,
    Marble, CityM,
    TempleM,
    Movement2,
    Marble, CityM,
    TempleM
  ) // 12: Temple    3,0,0 1,-4 1:1A 4T 6C 
  
  moves = List(
    Bellona,
    Marble,
    Iron,
    TempleM,
    Movement2, CityM,
    Marble,
    TempleM,
    Marble, CityM,
    Movement1, CityG,
    Gold,
    Marble,
    TempleG
  ) // 12: Temple     2,0,1 2,-3 1:1A 4T 6C 

  
  val initRessouce = Ressource(amount = 3, cities = 1, temples = 0)
  val initState = State(0, 0, 0, 0, initRessouce, initRessouce, initRessouce, InitialWS)
  
  val states = moves.scanLeft(initState)((state, m) => m.wheelMove >=> m.action apply state)
  
  println(showGame(moves, states))
  
  
  def showGame(moves: List[Move], states: List[State]): String = {
    def padRight(s: String, n: Int): String = s.padTo(n, " ").mkString
    def padLeft(s: String, n: Int): String = padRight(s.reverse, n).reverse

    val turns = moves zip states filterNot (_._1.ws == SubActionWS) map (_._2)
    (turns ::: List(states.last)).zipWithIndex.tail map {
      case (s, i) => padLeft(i + ": ", 4) + padRight(s.wheelState.toString.replace("WS", ""), 9) + " " + showState(s)
    } mkString "\n"
  }

  def showState(s: State): String = {
    import s._
    s"""
      ${marble.amount},${iron.amount},${gold.amount}
      ${coins},${fees}
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
  fees: Int,
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
  case object Movement1WS extends WheelState
  case object IronWS extends WheelState
  case object TempleWS extends WheelState
  case object GoldWS extends WheelState
  case object Movemnt2WS extends WheelState
  case object ArmyWS extends WheelState
  case object SubActionWS extends WheelState
  case object InitialWS extends WheelState
  val wheel = List(MarbleWS, ScienceWS, Movement1WS, IronWS, TempleWS, GoldWS, Movemnt2WS, ArmyWS)
  val wheel2 = wheel ::: wheel
}

class Move(val action: State => State, val ws: WheelState) {
  val wheelMove: State => State = { s =>
    if(s.wheelState == InitialWS)
      wheelState.set(ws)(s)
    else if(ws == SubActionWS)
      s
    else {
      val distance = wheel2.dropWhile(_ != s.wheelState).tail.takeWhile(_ != ws).size
      val cost = 0 max (1 + distance - 3)
      wheelState.set(ws) >=> fees.modify(_ - cost) apply s
    }
  }
}

object Moves {
  def collect(lens: Lens[State, Ressource]): State => State =
    lens.modify(rs => amount.modify(_ + rs.cities + 2 * rs.temples)(rs)) >=> coins.modify(_ + 1)
  
  def spend(n: Int, ressource: Lens[State, Ressource]): State => State = { s =>
    val ressources: Int = ressource composeLens amount get s
    val spendRessources = (ressource composeLens amount).modify(_ - (ressources min n))
    val spendCoins = coins.modify(_ - (0 max (n - ressources)))
    spendRessources >=> spendCoins apply s
  }
  
  def untilBroke(m: State => State): State => State =
    s => if(m(s).coins < 0) s else untilBroke(m)(m(s))
  
  def buildCity(ressource: Lens[State, Ressource]): State => State = { s =>
    val buildCity = ressource composeLens cities modify (_ + 1)
    val spendRessources = spend(1, marble) >=> spend(1, iron) >=> spend(1, gold)
    val useArmy = actives.modify(_ - 1) >=> idles.modify(_ + 1)
    if(s.actives == 0) s else buildCity >=> spendRessources >=> useArmy apply s
  }
  
  def buildTemples(ressource: Lens[State, Ressource]): State => State = { s =>
    val availableMarble = ((s.marble.amount + s.coins) / 5)
    val availableCities = (ressource composeLens cities).get(s) - (ressource composeLens temples).get(s)
    val n = availableMarble min availableCities
    spend(n * 5, marble) >=> (ressource composeLens temples).modify(_ + n) apply s
  }
  
  case object Marble extends Move(collect(marble), MarbleWS)
  case object Iron extends Move(collect(iron), IronWS)
  case object Gold extends Move(collect(gold), GoldWS)
  case object Science extends Move(identity, ScienceWS)
  case object Movement1 extends Move(s => actives.modify(_ + s.idles) >=> idles.set(0) apply s, Movement1WS)
  case object Movement2 extends Move(s => actives.modify(_ + s.idles) >=> idles.set(0) apply s, Movemnt2WS)
  case object TempleM extends Move(buildTemples(marble), TempleWS)
  case object TempleI extends Move(buildTemples(iron), TempleWS)
  case object TempleG extends Move(buildTemples(gold), TempleWS)
  case object Bellona extends Move(Army.action >=> idles.modify(_ + 1), ArmyWS)
  case object Army extends Move(untilBroke(spend(2, iron) >=> idles.modify(_ + 1)), ArmyWS)
  case object CityM extends Move(buildCity(marble), SubActionWS)
  case object CityI extends Move(buildCity(iron), SubActionWS)
  case object CityG extends Move(buildCity(gold), SubActionWS)
}

object Symbols {
  import scala.language.implicitConversions
  implicit class Function1Extensions[-T1, +R](f : T1 => R) {
    def >=>[A](g: (R) => A): (T1) => A = f andThen g
    def <=<[A](g: (A) => T1): (A) => R = f compose g
  }
}

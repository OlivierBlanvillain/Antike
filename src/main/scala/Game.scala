package antike

import monocle.macros.Lenses
import monocle.Lens

import State._
import Ressource._
import Moves._
import WheelState._
import Utils._

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

class Move(val ws: WheelState, val action: State => State) {
  def apply(previousState: State): State = {
    val fee: State => State = (previousState.wheelState, ws) match {
      case (_, SubActionWS) => identity
      case (TempleWS, TempleWS) => identity
      case (InitialWS, newWS) => wheelState set newWS
      case (prevWs, newWS) =>
        val prevIndex = infinitWheel.indexOf(prevWs)
        val newIndex = infinitWheel.indexOf(newWS, prevIndex + 1)
        val cost = 0 max ((newIndex - prevIndex) - 3)
        wheelState set newWS andThen (fees modify (_ - cost))
    }
    fee andThen action apply previousState
  }
}

object Moves {
  def collect(ressource: RessourceLens): State => State = chain(
    ressource.modify(r => amount.modify(_ + r.cities + 2 * r.temples)(r)),
    coins.modify(_ + 1))
  
  def spend(amountSpent: Int, ressource: RessourceLens): State => State = { s =>
    val initialAmount = ressource composeLens amount get s
    val newAmount = 0 max (initialAmount - amountSpent)
    val initialCoins = s.coins
    // Since initialAmount + initialCoins - amountSpent = newAmount + newCoins
    val newCoins = initialAmount + initialCoins - amountSpent - newAmount
    ressource composeLens amount set newAmount andThen (coins set newCoins) apply s
  }
  
  def untilBroke(m: State => State): State => State = s =>
    if(m(s).coins < 0) s else untilBroke(m)(m(s))
  
  def buildCity(ressource: RessourceLens): State => State = chain(
    ressource composeLens cities modify (_ + 1),
    spend(1, marble),
    spend(1, iron),
    spend(1, gold),
    actives.modify(_ - 1),
    idles.modify(_ + 1))
  
  def buildTemple(ressource: RessourceLens): State => State = chain(
    ressource composeLens temples modify (_ + 1),
    spend(5, marble))
  
  case object Marble extends    Move(MarbleWS, collect(marble))
  case object Iron extends      Move(IronWS, collect(iron))
  case object Gold extends      Move(GoldWS, collect(gold))
  case object Science extends   Move(ScienceWS, identity)
  case object Movement1 extends Move(Movement1WS, { s => actives.modify(_ + s.idles) andThen idles.set(0) apply s })
  case object Movement2 extends Move(Movemnt2WS, Movement1.action)
  case object TempleM extends   Move(TempleWS, buildTemple(marble))
  case object TempleI extends   Move(TempleWS, buildTemple(iron))
  case object TempleG extends   Move(TempleWS, buildTemple(gold))
  case object TempleM2 extends  Move(TempleWS, buildTemple(marble) andThen buildTemple(marble))
  case object TempleI2 extends  Move(TempleWS, buildTemple(iron) andThen buildTemple(iron))
  case object TempleG2 extends  Move(TempleWS, buildTemple(gold) andThen buildTemple(gold))
  case object TempleM3 extends  Move(TempleWS, buildTemple(marble) andThen buildTemple(marble) andThen buildTemple(marble))
  case object TempleI3 extends  Move(TempleWS, buildTemple(iron) andThen buildTemple(iron) andThen buildTemple(iron))
  case object TempleG3 extends  Move(TempleWS, buildTemple(gold) andThen buildTemple(gold) andThen buildTemple(gold))
  case object Bellona extends   Move(ArmyWS, chain(Army.action, idles.modify(_ + 1)))
  case object Army extends      Move(ArmyWS, untilBroke(spend(2, iron) andThen idles.modify(_ + 1)))
  case object CityM extends     Move(SubActionWS, buildCity(marble))
  case object CityI extends     Move(SubActionWS, buildCity(iron))
  case object CityG extends     Move(SubActionWS, buildCity(gold))
}

object WheelState {
  sealed trait WheelState
  case object IronWS extends WheelState
  case object TempleWS extends WheelState
  case object GoldWS extends WheelState
  case object Movemnt2WS extends WheelState
  case object ArmyWS extends WheelState
  case object MarbleWS extends WheelState
  case object ScienceWS extends WheelState
  case object Movement1WS extends WheelState
  // Special cases:
  case object SubActionWS extends WheelState
  case object InitialWS extends WheelState
  val gameWheel = List(IronWS, TempleWS, GoldWS, Movemnt2WS, ArmyWS, MarbleWS, ScienceWS, Movement1WS)
  val infinitWheel = gameWheel ::: gameWheel
}

object Utils {
  implicit val wheelStateOrdering: Ordering[WheelState] = Ordering.by(_.toString)
  implicit val ressourceOrdering: Ordering[Ressource] = Ordering.by(Ressource unapply _)
  implicit val stateOrdering: Ordering[State] = Ordering.by(State unapply _)
  def chain[A](f: A => A*) = Function.chain(f)
  type RessourceLens = Lens[State, Ressource]
}

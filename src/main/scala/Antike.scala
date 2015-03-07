package antike

import scala.Function.tupled
import monocle.Lens

import State._
import Ressource._
import Moves._
import WheelState._
import Utils._

object Antike extends App {
  val initRessouce = Ressource(amount = 3, cities = 1, temples = 0)
  val initState = State(0, 0, 0, 0, initRessouce, initRessouce, initRessouce, InitialWS)

  def isLegal(s: State) =
    s.coins >= 0 && s.actives >= 0 &&
    s.marble.amount + s.iron.amount + s.gold.amount + s.coins + s.fees >= 0 &&
    s.marble.cities >= s.marble.temples && s.iron.cities >= s.iron.temples && s.gold.cities >= s.gold.temples
  val allMoves = List(Marble, Iron, Gold, Movement1, Movement2, TempleG, TempleM, TempleM2, TempleM3, TempleI, TempleI2, TempleI3, Bellona, CityM, CityI)
  def nextMoves(s: State) = allMoves map (_(s)) filter isLegal
  
  def templesWanted(n: Int, r: Lens[State, Ressource])(s: State): Int = {
    val currentCities = r composeLens cities get s
    val currentTemples = r composeLens temples get s
    val templesNeeded = 0 max (n - currentCities)
    val cityNeeded = 0 max (templesNeeded - (currentTemples - currentCities))
    val activesNeeded = if(s.actives < cityNeeded) 1 else 0
    templesNeeded + cityNeeded + activesNeeded
  }

  // val moves = List(Bellona, Movement1, CityM, Gold, CityM, Marble, Iron, Movement2, CityM, Marble, TempleM2)
  val moves = List(Bellona, Movement1, CityM, Gold, CityM, Marble, TempleM, Marble, TempleM, Marble, TempleM)

  // val intermediate = moves.scanLeft(initState)((state, move) => move(state))
  val allStates = AStar.searchAll(initState)(nextMoves) { s =>
  // val allStates = AStar.searchAll(intermediate.last)(nextMoves) { s =>
    templesWanted(4, marble)(s)
  }
  allStates.foreach(s => println(showGame(s)))
  
  println()
  allStates.map(_.last).foreach(s => println(showState(s)))
  
  def showGame(states: List[State]): String = {
    // val turns = states
    def getMove(s1: State, s2: State): Move = allMoves.find(_(s1) == s2).get
    val moves = states zip states.tail map tupled(getMove)
    val turns = (moves zip states filterNot (_._1.ws == SubActionWS) map (_._2)) ::: List(states.last)
    val turnsString = turns.zipWithIndex map { case (s, i) =>
      padLeft(i + ": ", 4) + padRight(s.wheelState.toString.replace("WS", ""), 10) + " " + showState(s)
    } mkString "\n"
    "\n" + "val moves = " + moves + "\n\n" + turnsString
  }

  def padRight(s: String, n: Int): String = s.padTo(n, " ").mkString
  def padLeft(s: String, n: Int): String = padRight(s.reverse, n).reverse
  def showState(s: State): String = {
    import s._
    s"""
      ${marble.amount},${iron.amount},${gold.amount}
      ${coins},${fees}
      ${actives}:${idles}A
      ${marble.temples + iron.temples + gold.temples}T
      ${marble.cities + iron.cities + gold.cities}C
      (${marble.amount + iron.amount + gold.amount + coins + fees})
    """ split "\n" map (_.trim) mkString " "
  }
}

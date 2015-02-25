import monocle.macros.Lenses
import monocle.Lens

object Antike extends App {
  val moves: List[Move] = List(
    Marble,
    Temple,
    Marble,
    Gold,
    Temple)
  
  val initRessouce = SubState(res = 3, cities = 1, temples = 0)
  val initState = State(0, 0, initRessouce, initRessouce, initRessouce)
  val states = moves.scanLeft(initState)((state, move) => move f state)
  val game = moves.zip(states.tail).zipWithIndex.map {
    case ((move, state), turn) => s"Turn ${turn + 1}: $move\t($state)" }
  println(game mkString "\n")
}

@Lenses
case class State(actives: Int, idles: Int, marbleSS: SubState, ironSS: SubState, goldSS: SubState) {
  override def toString: String = s"""
    ${marbleSS.res},${ironSS.res},${goldSS.res} ${actives + idles}A
    ${marbleSS.temples + ironSS.temples + goldSS.temples}T
    ${marbleSS.cities + ironSS.cities + goldSS.cities}C
  """.split("\n").map(_.trim).mkString(" ").trim
}

case class SubState(res: Int, cities: Int, temples: Int)

class Move(val f: State => State)

class GetRessource(l: Lens[State, SubState]) extends Move(
  l.modify { rs => import rs._
    copy(res = res + cities + 3 * temples + 1)
  }
)
case object Iron extends GetRessource(State.ironSS)
case object Gold extends GetRessource(State.goldSS)
case object Marble extends GetRessource(State.marbleSS)
case object Science extends Move(identity)
case object Move extends Move(s => s.copy(actives = s.actives + s.idles, idles = 0))
case object Temple extends Move(State.marbleSS.modify { ms => import ms._
  val newTemples = (res / 5) min cities
  copy(temples = temples + newTemples, res = res - newTemples * 5)
})
case object Army extends Move ({ s => import s._
  copy(idles = idles + ironSS.res / 2, ironSS = ironSS.copy(res = ironSS.res % 2))
})

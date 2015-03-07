package antike

import scala.collection.immutable.Queue
import org.scalacheck._
import org.scalacheck.Prop.forAll

object AStarTest extends Properties(AStar.getClass.toString) {
  type Square = (Int, Int)

  def nextKnightPos(s: Square): List[Square] = {
    val moves = List((1, 2), (1, -2), (-1, 2), (-1, -2), (2, 1), (2, -1), (-2, 1), (-2, -1))
    def isValid(move: Square) = move._1 > 0 && move._1 < 9 && move._2 > 0 && move._2 < 9
    moves map (m => (m._1 + s._1, m._2 + s._2)) filter isValid
  }
  
  def mkHeuristic(a: Square)(b: Square): Int =
    if(a == b) 0 else 1 + ((a._1 - b._1).abs max (a._2 - b._2).abs) / 2
  
  def knightsShortestPath(init: Square, goal: Square): List[Square] =
    AStar.searchAll(init)(nextKnightPos)(mkHeuristic(goal)).headOption.getOrElse(Nil)
  
  def bfs[A](start: A)(goal: A => Boolean)(next: A => List[A]): List[A] = {
    def bfs0(queue: Queue[A], seen: Set[A], tracks: Map[A, A]): List[A] = {
      queue.headOption match {
        case None => Nil
        case Some(node) if goal(node) => AStar.findPath(tracks)(node)
        case Some(node) if seen contains node => bfs0(queue.tail, seen, tracks)
        case Some(node) =>
          val successors = next(node) filterNot seen.contains
          bfs0(queue.tail ++ successors, seen + node, tracks ++ successors.map((_, node)))
      }
    }
    bfs0(Queue(start), Set.empty, Map.empty)
  }
  
  val squares = for { x <- Gen.choose(1, 8); y <- Gen.choose(1, 8) } yield (x, y)
  
  property("path starts with initial square") = forAll(squares, squares) { (from, to) =>
    knightsShortestPath(from, to).head == from
  }

  property("path ends with goal square") = forAll(squares, squares) { (from, to) =>
    knightsShortestPath(from, to).last == to
  }

  property("no path for invalid goal") = forAll(squares) {
    knightsShortestPath(_, (-1, -1)).isEmpty
  }

  property("path is shortest") = forAll(squares, squares) { (from, to) =>
    knightsShortestPath(from, to).size == bfs(from)(to.==)(nextKnightPos).size
  }
}

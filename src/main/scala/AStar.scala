package antike

import scala.collection.immutable.TreeSet
import annotation.tailrec

object AStar {
  def searchAll[A](start: A)(next: A => List[A])(h: A => Int)(implicit o: Ordering[A]): List[List[A]] = {
    @tailrec
    def search0(
        pq: TreeSet[(Int, (A, Int))],
        seen: Set[A],
        gscore: Map[A, Int],
        tracks: Map[A, A],
        goals: List[A],
        maxCost: Option[Int]): List[List[A]] = {
      pq.headOption match {
        case None =>
          println("seen: " + seen.size)
          goals map findPath(tracks)
        case Some((_, (node, _))) if seen contains node =>
          search0(pq.tail, seen, gscore, tracks, goals, maxCost)
        case Some((costToGoal, (node, cost))) if(costToGoal == cost) =>
          search0(pq.tail, seen + node, gscore, tracks, node :: goals, Some(cost))
        case Some((_, (node, cost))) =>
          val successors = next(node) filterNot seen.contains filterNot (gscore get _ exists (_ <= cost + 1))
          val successorsPQ = successors.map(a => (h(a) + cost + 1, (a, cost + 1)))
          val pq0 = maxCost.fold(successorsPQ)(max => successorsPQ filter (_._1 <= max))
          val gscore0 = gscore ++ successors.map((_, cost + 1))
          val tracks0 = tracks ++ successors.map((_, node))
          search0(pq.tail ++ pq0, seen + node, gscore0, tracks0, goals, maxCost)
      }
    }
    // Relies on scala.math.Ordering.Tuple2 to compare _1s before _2s.
    search0(TreeSet((h(start), (start, 0))), Set.empty, Map((start, 0)), Map.empty, Nil, None)
  }
  
  def findPath[A](tracks: Map[A, A])(node: A): List[A] =
    (tracks get node map findPath(tracks) getOrElse Nil) ::: List(node)
}

package antike

import scala.collection.immutable.TreeSet
import annotation.tailrec

object AStar {
  def search[A](start: A)(goal: A => Boolean)(next: A => List[A])(h: A => Int)(implicit o: Ordering[A]): List[A] = {
    @tailrec
    def search0(pq: TreeSet[(Int, (A, Int))], seen: Set[A], gscore: Map[A, Int], tracks: Map[A, A]): List[A] = {
      pq.headOption.map(_._2) match {
        case None => Nil
        case Some((node, _)) if goal(node) => findPath(tracks)(node)
        case Some((node, _)) if seen contains node => search0(pq.tail, seen, gscore, tracks)
        case Some((node, cost)) =>
          val successors = next(node) filterNot seen.contains filterNot (gscore get _ exists (_ <= cost + 1))
          val pq0 = pq.tail ++ successors.map(a => (h(a) + cost + 1, (a, cost + 1)))
          val gscore0 = gscore ++ successors.map((_, cost + 1))
          val tracks0 = tracks ++ successors.map((_, node))
          search0(pq0, seen + node, gscore0, tracks0)
      }
    }
    // Relies on scala.math.Ordering.Tuple2 to compare _1s before _2s.
    search0(TreeSet((h(start), (start, 0))), Set.empty, Map((start, 0)), Map.empty)
  }
  
  def findPath[A](tracks: Map[A, A])(node: A): List[A] =
    (tracks get node map findPath(tracks) getOrElse Nil) ::: List(node)
}

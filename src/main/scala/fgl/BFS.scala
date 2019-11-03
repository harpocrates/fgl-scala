package fgl

import scala.collection.immutable.Queue

object BFS{

  def bfsWith[G[_,_],V,E,C](graph: G[V,E], visitNode: Context[V,E] => C, toVisit: Queue[Node])(implicit
    impl: Graph[G],
  ): LazyList[C] = toVisit.dequeueOption match {
    case None => LazyList.empty
    case Some((next, thenVisit)) =>
      graph.elimAny {
        case None => LazyList.empty
        case Some((ctx, restGraph)) => visitNode(ctx) #:: bfsWith(
          restGraph,
          visitNode,
          graph.suc(ctx.node) ++: thenVisit
        )
      }
  }

}

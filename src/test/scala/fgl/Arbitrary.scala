package fgl

import scala.collection.mutable
import org.scalacheck.{Arbitrary, Gen, Shrink}

case class GraphNodesEdges[A,B](
  nodes: List[LabelledNode[A]],
  edges: List[LabelledEdge[B]]
) {

  def toGraph[G[_,_]](implicit impl: Graph[G]): G[A,B] =
    impl.mkGraph(nodes, edges)

  private def pruneBogusEdges: GraphNodesEdges[A,B] = {
    val nodeSet: Set[Node] = nodes.map(_.node).toSet
    GraphNodesEdges(
      nodes,
      edges.filter(e => nodeSet.contains(e.from) && nodeSet.contains(e.to))
    )
  }

  /** Produce a stream of shrunken graphs */
  def shrink = Shrink
    .shrink(nodes -> edges)
    .map { case (ns, es) => GraphNodesEdges(ns, es) }
    .map(_.pruneBogusEdges)

  /** For all non-circular edges, make sure there is a reverse in the graph */
  def makeUndirected: GraphNodesEdges[A,B] = {
    val newEdges = Set.newBuilder[LabelledEdge[B]]
    for (edge <- edges) {
      newEdges += edge
      if (edge.from == edge.to) {
        newEdges += edge.reversed
      }
    }
    GraphNodesEdges(
      nodes,
      newEdges.result().toList
    )
  }

  /** Remove any circular edges from the graph */
  def makeNoCircular: GraphNodesEdges[A,B] =
    GraphNodesEdges(
      nodes,
      edges.filter(e => e.to != e.from)
    )

  /** Remove edges so that any given node has only one edge pointing to it */
  def makeSingleIncomingEdge: GraphNodesEdges[A,B] =
    GraphNodesEdges(
      nodes,
      edges.groupBy(_.to).values.map(_.head).toList
    )

  /** Remove edges so that any given node has only one edge pointing from it */
  def makeSingleOutgoingEdge: GraphNodesEdges[A,B] =
    GraphNodesEdges(
      nodes,
      edges.groupBy(_.to).values.map(_.head).toList
    )
}

object GraphNodesEdges {

  implicit def apply[A, B](implicit
    arbA: Arbitrary[A],
    arbB: Arbitrary[B]
  ): Arbitrary[GraphNodesEdges[A,B]] =
    Arbitrary(
      ArbitraryGraph.genGraphNodesEdges(
        arbA.arbitrary,
        arbB.arbitrary
      )
    )

}

object ArbitraryGraph {

  lazy val genNode: Gen[Node] =
    Gen.Choose.chooseInt.choose(Int.MinValue, Int.MaxValue).map(Node(_))

  def genLabelledNode[A](genA: Gen[A]): Gen[LabelledNode[A]] =
    for {
      node <- genNode
      a <- genA
    } yield LabelledNode(node, a)

  /** The nodes have unique IDs */
  def genLabelledNodes[A](genA: Gen[A]): Gen[List[LabelledNode[A]]] = {
    val usedIds = mutable.Set.empty[Int]
    Gen.listOf(
      for {
        node <- genNode
        if (usedIds.add(node.id))
        a <- genA
      } yield LabelledNode(node, a)
    )
  }


  /** The edges connect the nodes argument */
  def genLabelledEdges[A, B](genB: Gen[B], nodes: List[LabelledNode[A]]): Gen[List[LabelledEdge[B]]] = {
    if (nodes.isEmpty) {
      return Gen.const(List.empty)
    }
    val nodeGen: Gen[Node] = Gen.oneOf(nodes).map(_.node)
    val seenEdges = mutable.Set.empty[LabelledEdge[B]]
    Gen.listOf(
      for {
        from <- nodeGen
        b <- genB
        to <- nodeGen
        edge = LabelledEdge(from, b, to)
        if (seenEdges.add(edge))
      } yield LabelledEdge(from, b, to)
    )
  }

  def genGraphNodesEdges[A, B](genA: Gen[A], genB: Gen[B]): Gen[GraphNodesEdges[A,B]] = {
    val genNodes = genLabelledNodes(genA)
    val genEdges = genLabelledEdges(genB, _: List[LabelledNode[A]])
    for {
      nodes <- genNodes
      edges <- genEdges(nodes)
    } yield GraphNodesEdges(nodes, edges)
  }

  /** Synthesize an arbitrary graph instance */
  def apply[G[_,_], A, B](
    restrict: GraphNodesEdges[A,B] => GraphNodesEdges[A,B] = (g: GraphNodesEdges[A,B]) => g
  )(implicit
    impl: Graph[G],
    arbA: Arbitrary[A],
    arbB: Arbitrary[B]
  ): Arbitrary[G[A,B]] = Arbitrary(
    genGraphNodesEdges(arbA.arbitrary, arbB.arbitrary).map(restrict).map(_.toGraph)
  )
}

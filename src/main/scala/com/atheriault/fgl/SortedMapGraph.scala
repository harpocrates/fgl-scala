package com.atheriault.fgl

import scala.collection.immutable.{SortedMap, TreeMap, SortedMultiDict}
import scala.collection.decorators._

/** Efficient representation for an immutable graph
 *
 *  @param treeMap sorted map from node to the context associated with the node
 */
case class SortedMapGraph[V,E](
  treeMap: SortedMap[Node, SortedContext[V,E]]
)

/** Efficient representation for a context of a [[SortedMapGraph]]
 *
 *  @param fromNodesMap nodes pointing to this node, and labels of those edges
 *  @param node the ID of the node represented by the context
 *  @param value the label on the node
 *  @param toNodesMap nodes pointed to by this node, and labels of those edges
 */
case class SortedContext[V,E](
  fromNodesMap: SortedMap[Node, Set[E]],
  node: Node,
  value: V,
  toNodesMap: SortedMap[Node, Set[E]],
) extends Context[V,E] {
  override def fromNode = Adj {
    fromNodesMap.view
      .flatMap { case (n, setE) => setE.view.map { _ -> n } }
      .toList
  }

  override def toNode = Adj {
    toNodesMap.view
      .flatMap { case (n, setE) => setE.view.map { _ -> n } }
      .toList
  }
  
  override def map[W,D](
    nodeMap: V => W,
    edgeMap: E => D,
  ) = SortedContext[W,D](
    fromNodesMap.view.mapValues(_.map(edgeMap)).to(SortedMap),
    node,
    nodeMap(value),
    toNodesMap.view.mapValues(_.map(edgeMap)).to(SortedMap),
  )
}


object SortedMapGraph {

  implicit object graphImpl extends Graph[SortedMapGraph] {

    def empty[V,E] = SortedMapGraph(TreeMap.empty[Node, SortedContext[V,E]])

    def isEmpty[V,E](graph: SortedMapGraph[V,E]): Boolean =
      graph.treeMap.isEmpty

    def `match`[V,E](
      node: Node,
      graph: SortedMapGraph[V,E],
    ): (Option[SortedContext[V, E]], SortedMapGraph[V,E]) =
      graph.treeMap.get(node) match {
        case None => (None, graph)
        case optCtx @ Some(nodeContext) =>
          (optCtx, cleanGraph(graph, nodeContext))
      }

    def matchAny[V,E](
      graph: SortedMapGraph[V,E]
    ): (SortedContext[V, E], SortedMapGraph[V,E]) = {
      val nodeContext = graph.treeMap.lastOption match {
        case Some((_, value)) => value
        case None => throw new IllegalArgumentException("Cannot match any on an empty graph")
      }
      (nodeContext, cleanGraph(graph, nodeContext))
    }

    /** Purge from a graph all references to stuff from the given context.
     *
     *  @param graph the graph to clean up
     *  @param nodeContext the context whose node and edges will be removed
     *  @return the new purged graph
     */
    @inline
    private def cleanGraph[V,E](
      graph: SortedMapGraph[V,E],
      nodeContext: SortedContext[V,E],
    ): SortedMapGraph[V,E] = {
      
      // Take the node out...
      val node = nodeContext.node
      var newTreeMap = graph.treeMap.removed(node)

      // Remove the other end of edges ending at the node we took out
      for (n <- nodeContext.fromNodesMap.keys) {
        val oldCtx: SortedContext[V,E] = newTreeMap(n)
        val newCtx: SortedContext[V,E] = oldCtx.copy(toNodesMap = oldCtx.toNodesMap.removed(node))
        newTreeMap = newTreeMap.updated(n, newCtx)
      }

      // Remove the other end of edges starting at the node we took out
      for (n <- nodeContext.toNodesMap.keys) {
        val oldCtx: SortedContext[V,E] = newTreeMap(n)
        val newCtx: SortedContext[V,E] = oldCtx.copy(fromNodesMap = oldCtx.fromNodesMap.removed(node))
        newTreeMap = newTreeMap.updated(n, newCtx)
      }

      SortedMapGraph(newTreeMap)
    }

    // TODO: good error message when a node refers to a node that doesn't exist
    //       (as opposed to just silently dropping that edge)
    def mkGraph[V,E](
      nodes: Iterable[LabelledNode[V]],
      edges: Iterable[LabelledEdge[E]],
    ): SortedMapGraph[V,E] = {

      // Group the edges
      val groupsForward: Map[Node, TreeMap[Node, Set[E]]] = edges
        .groupMapReduce(_.from)(l => TreeMap(l.to -> Set(l.label))) {
          case (map1, map2) => map1.mergeByKeyWith(map2) {
            case (None, Some(s2)) => s2
            case (Some(s1), None) => s1
            case (Some(s1), Some(s2)) => s1 intersect s2
          }
        }
      val groupsBackward: Map[Node, TreeMap[Node, Set[E]]] = edges
        .groupMapReduce(_.to)(l => TreeMap(l.from -> Set(l.label))) {
          case (map1, map2) => map1.mergeByKeyWith(map2) {
            case (None, Some(s2)) => s2
            case (Some(s1), None) => s1
            case (Some(s1), Some(s2)) => s1 intersect s2
          }
        }

      // Build the graph map 
      val builder = TreeMap.newBuilder[Node, SortedContext[V,E]]
      for (LabelledNode(node, value) <- nodes) {
        val from = groupsForward.getOrElse(node, TreeMap.empty[Node, Set[E]])
        val to = groupsBackward.getOrElse(node, TreeMap.empty[Node, Set[E]])
        val newCtx = SortedContext(from, node, value, to)
        builder += (node -> newCtx)
      }

      SortedMapGraph(builder.result())
    }

    def nodes[V,E](graph: SortedMapGraph[V,E]): Iterable[LabelledNode[V]] =
      graph.treeMap
        .view
        .map { case (k,v) => LabelledNode(k, v.value) }

    def edges[V,E](graph: SortedMapGraph[V,E]): Iterable[LabelledEdge[E]] =
      graph.treeMap
        .view
        .flatMap { case (w, ctx) =>
          ctx.toNodesMap
            .view
            .flatMap { case (v, setE) => setE.view.map { LabelledEdge(v, _, w) } }
        }

    def nodeCount[V,E](graph: SortedMapGraph[V,E]): Int =
      graph.treeMap.size

    def nodeRange[V,E](graph: SortedMapGraph[V,E]): (Node, Node) =
      (graph.treeMap.firstKey, graph.treeMap.lastKey)

    def combine[V,E](context: Context[V, E], graph: SortedMapGraph[V,E]): SortedMapGraph[V,E] = {
  
      val node = context.node
      var newGraph = graph.treeMap

      val fromNodesBuilder = SortedMultiDict.newBuilder[Node, E]
      val toNodesBuilder = SortedMultiDict.newBuilder[Node, E]

      // Update nodes supposed to be pointed to from the new node
      for ((edgeLbl, from) <- context.fromNode.edges) {
        fromNodesBuilder += from -> edgeLbl
        val oldCtx = newGraph(from)
        val newSet = oldCtx.toNodesMap.getOrElse(node, Set.empty).incl(edgeLbl)
        val newCtx = oldCtx.copy(toNodesMap = oldCtx.toNodesMap.updated(node, newSet))
        newGraph = newGraph.updated(from, newCtx)
      }

      // Update nodes supposed to be pointing to the new node
      for ((edgeLbl, to) <- context.toNode.edges) {
        toNodesBuilder += to -> edgeLbl
        val oldCtx = newGraph(to)
        val newSet = oldCtx.fromNodesMap.getOrElse(node, Set.empty).incl(edgeLbl)
        val newCtx = oldCtx.copy(fromNodesMap = oldCtx.fromNodesMap.updated(node, newSet))
        newGraph = newGraph.updated(to, newCtx)
      }
    
      // Add the new node
      SortedMapGraph(newGraph.updated(node, SortedContext(
        fromNodesMap = fromNodesBuilder.result().sets,
        node = context.node,
        value = context.value,
        toNodesMap = toNodesBuilder.result().sets,
      )))
    }
  }
}


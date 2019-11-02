package com.atheriault.fgl

import scala.collection.mutable

case class Node(id: Int) extends AnyVal
object Node {
  implicit val ordering = Ordering.Int.on[Node](_.id)
}
trait Edge {
  val from: Node
  val to: Node
}

case class LabelledNode[V](node: Node, value: V)
case class LabelledEdge[E](from: Node, label: E, to: Node) extends Edge {
  def reversed = LabelledEdge(to, label, from)
}

case class Adj[E](edges: List[(E, Node)]) {
  /** Map the labels, but preserve the structure */
  def map[B](
    edgeMap: E => B,
  ): Adj[B] = Adj(edges.map { case (e, n) => edgeMap(e) -> n })
}
object Adj {
  def empty[E]: Adj[E] = Adj(List.empty)
}


/** Context for a graph containing a node and all the edges connected to it */
trait Context[V, E] {

  /** The adjacency list of edges pointing to this node */
  def toNode: Adj[E]

  /** The node ID */
  def node: Node

  /** The value stored on the node */
  def value: V

  /** The adjacency list of edges pointing from this node */
  def fromNode: Adj[E]

  /** Map the labels, but preserve the structure
   *
   *  @param nodeMap how to map labels on nodes
   *  @param edgeMap how to map labels on edges
   *  @return the mapped context
   */
  def map[W,D](
    nodeMap: V => W,
    edgeMap: E => D,
  ): Context[W,D]

  def asLabelledNode: LabelledNode[V] = LabelledNode(node, value)
}
object Context {
  def apply[V,E](
    toNode: Adj[E],
    node: Node,
    value: V,
    fromNode: Adj[E],
  ): Context[V,E] = SimpleContext(toNode, node, value, fromNode)
}

private[fgl] case class SimpleContext[V,E](
  toNode: Adj[E],
  node: Node,
  value: V,
  fromNode: Adj[E],
) extends Context[V,E] {

  override def map[W,D](
    nodeMap: V => W,
    edgeMap: E => D,
  ): SimpleContext[W,D] = SimpleContext(
    toNode.map(edgeMap),
    node,
    nodeMap(value),
    fromNode.map(edgeMap),
  )

}


/** Typeclass for static immutable graphs.
 *
 *  Based on <https://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.pdf>
 */
trait Graph[G[_,_]] {

  /** Create an empty graph
   *
   *  @return a new empty graph
   */
  def empty[V,E]: G[V,E]

  /** Check if a graph is empty
   *
   *  @param graph the graph that may be empty
   *  @return whether the graph is empty
   */
  def isEmpty[V,E](graph: G[V,E]): Boolean

  /** Try to match a particular node in the graph
   *
   *  @param node the ID of the node context to extract
   *  @param graph the graph from which to extract the context
   *  @return the context (if it could be extracted), and the remaining graph
   */
  def `match`[V,E](node: Node, graph: G[V,E]): (Option[Context[V, E]], G[V,E])

  /** Match on any node in the graph
   *
   *  @note the graph must be non-empty
   *
   *  @param graph the graph from which to extract a node
   *  @return the context extracted and the remaining graph
   */
  def matchAny[V,E](graph: G[V,E]): (Context[V, E], G[V,E])

  /** Create a graph from labelled nodes and edges
   *
   *  @note the edges must refer to nodes that are in the node list
   *
   *  @param nodes labelled list of nodes
   *  @param edges labelled list of edges
   *  @return a graph with those nodes and edges
   */
  def mkGraph[V,E](
    nodes: Iterable[LabelledNode[V]],
    edges: Iterable[LabelledEdge[E]],
  ): G[V,E]

  /** Get all the nodes in a graph
   *
   *  @param graph the graph whose nodes we are getting
   *  @return a list of labelled nodes
   */
  def nodes[V,E](graph: G[V,E]): Iterable[LabelledNode[V]]

  /** Get all the edges in a graph
   *
   *  @param graph the graph whose edges we are getting
   *  @return a list of labelled edges
   */
  def edges[V,E](graph: G[V,E]): Iterable[LabelledEdge[E]]

  /** Get the total number of nodes in the graph
   *
   *  @param graph the graph whose nodes are being tallied up
   *  @return total number of nodes
   */
  def nodeCount[V,E](graph: G[V,E]): Int

  /** Get the smallest and largest nodes in the graph (according to the
   *  ordering defined in [[Node.ordering]]).
   *
   *  @note the graph must be non-empty
   *
   *  @param graph the graph
   *  @return the minimum and maximum node
   */
  def nodeRange[V,E](graph: G[V,E]): (Node, Node)

  /** Create a new graph by adding the provided context to the graph.
   *
   *  @note node IDs in edges in the context should already be in the graph
   *
   *  @param context the new node and its edges into the existing graph
   *  @param graph the old graph
   *  @return a graph with a new node and edges from the context
   */
  def combine[V,E](context: Context[V, E], graph: G[V,E]): G[V,E]
}

object Graph {

  /** Create a new empty graph */
  def empty[G[_,_],V,E](implicit impl: Graph[G]): G[V,E] = impl.empty

  /** Create a graph from labelled nodes and edges.
   *
   *  @see [[Graph.mkGraph]]
   */
  def apply[G[_,_],V,E](
    nodes: Iterable[LabelledNode[V]],
    edges: Iterable[LabelledEdge[E]],
  )(implicit impl: Graph[G]): G[V,E] = impl.mkGraph(nodes, edges)

  /** Create an unlabelled graph from nodes and edges.
   *
   *  @see [[Graph.mkGraph]]
   */
  def unlabelled[G[_,_]](
    nodes: Iterable[Node],
    edges: Iterable[Edge],
  )(implicit impl: Graph[G]): G[Unit, Unit] = impl.mkGraph(
    nodes.map(n => LabelledNode(n, ())),
    edges.map(e => LabelledEdge(e.from, (), e.to)),
  )

  def newBuilder[G[_,_],V,E](implicit impl: Graph[G]) =
    new GraphBuilder[G,V,E](impl)
}


/** Efficiently create new graphs
 *
 *  {{{
 *  val graph = {
 *    val builder = Graph.newBuilder[SortedMapGraph, String, String]
 *
 *    val n1 = exampleBuilder.freshNode("a")
 *    val n2 = exampleBuilder.freshNode("b")
 *    val n3 = exampleBuilder.freshNode("c")

 *    builder += LabelledEdge(n1, "right", n2)
 *    builder += LabelledEdge(n2, "down", n3)
 *    builder += LabelledEdge(n2, "left", n1)
 *    builder += LabelledEdge(n3, "up", n1)
 *
 *    builder.result()
 *  }
 *  }}}
 *
 */
class GraphBuilder[G[_,_],V,E](private val graphImpl: Graph[G]) extends mutable.Builder[LabelledEdge[E],G[V,E]] {

  private var nextNodeId: Int = 0
  private val nodes = Seq.newBuilder[LabelledNode[V]]
  private val edges = Seq.newBuilder[LabelledEdge[E]]

  /** Create and add a fresh node to the graph
   *
   *  @param label the label to put on the node
   *  @return the ID of the node (useful for making edges!)
   */
  def freshNode(label: V): Node = {
    val newNode = Node(nextNodeId)
    nextNodeId += 1
    nodes += LabelledNode(newNode, label)
    newNode
  }

  override def addOne(edge: LabelledEdge[E]): this.type = {
    edges += edge
    this
  }

  override def clear(): Unit = {
    nodes.clear()
    edges.clear()
  }

  override def result(): G[V,E] =
    graphImpl.mkGraph(nodes.result(), edges.result())
}

object GraphBuilderExample {

  val exampleBuilder = Graph.newBuilder[SortedMapGraph, String,String]
  val n1 = exampleBuilder.freshNode("a")
  val n2 = exampleBuilder.freshNode("b")
  val n3 = exampleBuilder.freshNode("c")

  exampleBuilder += LabelledEdge(n1, "right", n2)
  exampleBuilder += LabelledEdge(n2, "down", n3)
  exampleBuilder += LabelledEdge(n2, "left", n1)
  exampleBuilder += LabelledEdge(n3, "up", n1)

}

object Main extends App {

  GraphBuilderExample.exampleBuilder.result().dotFile("temp.dot").svg("temp.svg")
}


package com.atheriault.fgl

import scala.util.Random

object Example {

  /** Make a graph shaped like a cycle
   *
   *  @param n number of nodes in the cycle
   *  @return a graph where each node in the cycle points to the next
   */
  def cycle[G[_,_] : Graph](n: Int): G[Int,Unit] = {
    val builder = Graph.newBuilder[G, Int, Unit]

    val nodes = (1 to n).map { builder.freshNode(_) }

    for ((n,m) <- (nodes zip nodes.tail)) {
      builder += LabelledEdge(n, (), m)
    }
    builder += LabelledEdge(nodes.last, (), nodes.head)

    builder.result()
  }

  /** Make a graph shaped like a star
   *
   *  @param n number of nodes in the star
   *  @return a graph where all but the first node are pointed to by the first
   */
  def star[G[_,_] : Graph](n: Int): G[Int, Unit] = {
    val builder = Graph.newBuilder[G, Int, Unit]

    val nodes = (1 to n).map { builder.freshNode(_) }
    nodes.tail.foreach { builder += LabelledEdge(nodes.head, (), _) }

    builder.result()
  }

  /** Figure 22.7 of Cormen/Leiserson/Rivest's Introduction to Algorithms 3ed */
  def clr227[G[_,_] : Graph]: G[String, Unit] = {
    val builder = Graph.newBuilder[G, String, Unit]

    val n1 = builder.freshNode("undershorts")
    val n2 = builder.freshNode("socks")
    val n3 = builder.freshNode("watch")
    val n4 = builder.freshNode("pants")
    val n5 = builder.freshNode("shoes")
    val n6 = builder.freshNode("shirt")
    val n7 = builder.freshNode("belt")
    val n8 = builder.freshNode("tie")
    val n9 = builder.freshNode("jacket")

    builder += LabelledEdge(n1, (), n4)
    builder += LabelledEdge(n1, (), n5)
    builder += LabelledEdge(n2, (), n5)
    builder += LabelledEdge(n4, (), n5)
    builder += LabelledEdge(n4, (), n7)
    builder += LabelledEdge(n6, (), n7)
    builder += LabelledEdge(n6, (), n8)
    builder += LabelledEdge(n7, (), n9)
    builder += LabelledEdge(n8, (), n9)

    builder.result()
  }

  /** Make a random graph
   *
   *  {{{
   *  random(
   *    numberNodes = 20,
   *    numberEdges = 40,
   *    nodeLabel = Random.alphanumeric.take(5).mkString,
   *    edgeLabel = Random.nextInt(100),
   *  )
   *  }}}
   *
   *  @param numberNodes total number of nodes to put in the graph
   *  @param numberEdges total number of edges to put in the graph
   *  @param nodeLabel a by-name expression to evaluate to get new node labels
   *  @param edgeLabel a by-name expression to evaluate to get new edge labels
   *  @return a random graph
   */
  def random[G[_,_] : Graph, V, E](
    numberNodes: Int,
    numberEdges: Int,
    nodeLabel: => V,
    edgeLabel: => E,
  ): G[V,E] = {
    val builder = Graph.newBuilder[G, V, E]

    val nodes = (1 to numberNodes)
      .map { _ => builder.freshNode(nodeLabel) }
      .toArray

    for (_ <- 1 to numberEdges) {
      builder += LabelledEdge(
        from = nodes(Random.nextInt(nodes.length)),
        label = edgeLabel,
        to = nodes(Random.nextInt(nodes.length))
      )
    }

    builder.result()
  }
}

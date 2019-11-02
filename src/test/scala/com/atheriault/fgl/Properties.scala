package com.atheriault.fgl

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalatest.prop.Configuration

abstract class GraphTests[G[_,_], V, E](name: String)
    extends Properties(name) with Configuration {

  implicit val arbNode = Arbitrary[Node] {
    Arbitrary.arbInt.arbitrary.map(Node(_))
  }

  implicit val impl: Graph[G]
  implicit val arbV: Arbitrary[V]
  implicit val arbE: Arbitrary[E]

  // Ensure a graph has the nodes with which it was created
  property("nodes") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    nodesEdges.toGraph.nodes.toSet == nodesEdges.nodes.toSet
  }
  
  // Ensure a graph has the edges with which it was created
  property("edges") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    nodesEdges.toGraph.edges.toSet == nodesEdges.edges.toSet
  }
  
  // Ensure the range of a graph matches the min & max of the nodes
  property("range") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    nodesEdges.nodes.nonEmpty ==> {
      val minNode = nodesEdges.nodes.map(_.node).min
      val maxNode = nodesEdges.nodes.map(_.node).max
      nodesEdges.toGraph.range == (minNode, maxNode)
    }
  }

  property("valid isEmpty") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    nodesEdges.nodes.isEmpty == nodesEdges.toGraph.isEmpty
  }

  // Ensure graphs with only nodes retain those nodes and have no edges
  property("only nodes") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val onlyNodes = nodesEdges.copy(edges = List.empty[LabelledEdge[E]])
    val onlyNodesGraph = onlyNodes.toGraph
    onlyNodesGraph.nodes.toSet == onlyNodes.nodes.toSet &&
      onlyNodesGraph.edges.isEmpty
  }

  // Making graphs with edges/nodes in a different order should work
  property("node/edge order") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph1 = nodesEdges.toGraph
    val graph2 = GraphNodesEdges(
      nodesEdges.nodes,
      nodesEdges.edges.reverse
    ).toGraph
    val graph3 = GraphNodesEdges(
      nodesEdges.nodes.reverse,
      nodesEdges.edges
    ).toGraph
    val graph4 = GraphNodesEdges(
      nodesEdges.nodes.reverse,
      nodesEdges.edges.reverse
    ).toGraph
    
    graph1 == graph2 && graph1 == graph3 && graph1 == graph4
  }

  // When a node is matched, it must actually be removed
  property("valid match") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    !graph.isEmpty ==> {
      forAll(Gen.oneOf(nodesEdges.nodes)) { lnode: LabelledNode[V] =>
        graph.elim(lnode.node) {
          case None => Prop(false) // we should've found the node
          case Some((ctx, restGraph)) =>
            all(
              // node should not be in the graph anymore
              !restGraph.nodes.toList.contains(ctx.node),

              // number of nodes should be one less than before
              restGraph.nodes.size == nodesEdges.nodes.size - 1,

              // Edges were previously in the graph
              all(ctx.toNode.edges.map[Prop] { case (_, n) =>
                graph.suc(n).toList.contains(ctx.node)
              }: _*),
              all(ctx.fromNode.edges.map[Prop] { case (_, n) =>
                graph.pre(n).toList.contains(ctx.node)
              }: _*),
              
              // Edges not in new graph
              all(ctx.toNode.edges.map[Prop] { case (_, n) =>
                !restGraph.suc(n).toList.contains(ctx.node)
              }: _*),
              all(ctx.fromNode.edges.map[Prop] { case (_, n) =>
                !restGraph.pre(n).toList.contains(ctx.node)
              }: _*)
            )
        }
      }
    }
  }

  // When any node is matched, the result is the same as if we had match that
  property("valid matchAny") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    !graph.isEmpty ==>
      graph.elimAny {
        case None => false // the graph is supposed to be non-empty
        case res @ Some((ctx, _)) => res == graph.elim(ctx.node)(identity)
      }
  }

  // New nodes should return nodes that aren't in the graph
  property("valid newNodes") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    forAll(Gen.posNum[Int]) { n: Int => 
      val newNodes = graph.newNodes(n)
      all(newNodes.map[Prop](!graph.contains(_)): _*) && newNodes.size == n
    }
  }

  // [[ufold]] should create a context for each node
  property("ufold all nodes") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    graph.nodes.toSet ==
      graph.ufold[Set[LabelledNode[V]]](
        (ctx, s) => s.incl(ctx.asLabelledNode),
        Set.empty
      )
  }

  // All nodes are contained in the graph
  property("graph contains all nodes") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    all(nodesEdges.nodes.map[Prop](n => graph.contains(n)): _*)
  }

  // All edges are contained in the graph
  property("graph contains all edges") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    all(nodesEdges.edges.map[Prop](e => graph.contains(e)): _*)
  }

  // if a node is contained, it should also be in the output of [[nodes]]
  property("contains in nodes") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    forAll { node: Node =>
      graph.contains(node) == graph.nodes.toSet.contains(node)
    }
  }

  // having a labelled edge in a graph is the same as [[contains]]
  property("contains in edges") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    forAll { (from: Node, label: E, to: Node) =>
      val edge = LabelledEdge(from, label, to)
      graph.contains(edge) == graph.edges.toSet.contains(edge)
    }
  }

  // extracting the context for a node, then merging it back should be identity
  property("valid merge") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    !graph.isEmpty ==> {
      forAll(Gen.oneOf(nodesEdges.nodes)) { lnode: LabelledNode[V] =>
        graph.elim(lnode.node) {
          case None => false // the node should be in the graph!
          case Some((ctx, restGraph)) => (ctx & restGraph) == graph
        }
      }
    }
  }

  // generalized mapping with the identity function should be identity
  property("gmap identity") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    graph.gmap(identity) == graph
  }

  // add just one extra node to the graph
  property("add a node") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    val Seq(id) = graph.newNodes(1)
    forAll { lbl: V =>
      val newNode = LabelledNode(id, lbl)
      val newGraph = graph + newNode
      newGraph.contains(id) &&
        newGraph.nodes.toSet == graph.nodes.toSet + newNode &&
        newGraph.edges.toSet == graph.edges.toSet
    }
  }

  // add just one extra edge to the graph
  property("add an edge") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    !graph.isEmpty ==> {
      val nodeGen = Gen.oneOf(nodesEdges.nodes).map(_.node)
      forAll(nodeGen, arbE.arbitrary, nodeGen) { (from: Node, lbl: E, to: Node) =>
        val newEdge = LabelledEdge(from, lbl, to)
        val newGraph = graph + newEdge
        newGraph.contains(newEdge) &&
          newGraph.nodes.toSet == graph.nodes.toSet &&
          newGraph.edges.toSet == graph.edges.toSet + newEdge
      }
    }
  }

  // remove a node, and check there are no edges referencing that node after
  property("remove a node") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    !graph.isEmpty ==> {
      forAll(Gen.oneOf(nodesEdges.nodes)) { lnode: LabelledNode[V] =>
        val newGraph = graph - lnode.node
        all(
          // the node is no longer in the graph
          !newGraph.contains(lnode),

          // the node is not any edges endpoint
          !newGraph.edges
            .flatMap(e => List(e.from, e.to)).toSet
            .contains(lnode.node)
        )
      }
    }
  }

  // remove an edges, and check that no nodes were also removed
  property("remove an edge") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    !nodesEdges.edges.isEmpty ==> {
      forAll(Gen.oneOf(nodesEdges.edges)) { ledge: LabelledEdge[E] =>
        val newGraph = graph.removeOne(ledge)
        all(
          // the edge is no longer in the graph
          !newGraph.contains(ledge),

          // all the nodes are still there
          newGraph.nodes.toSet == graph.nodes.toSet,

          // we have exatcly one less edge
          newGraph.edges.size == graph.edges.size - 1
        )
      }
    }
  }

  // check that making a graph directly is the same as inserting edges/nodes
  property("mkGraph inserts") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    
    val onlyNodes = nodesEdges.nodes.foldLeft[G[V,E]](Graph.empty)(_ + _)
    val newGraph = nodesEdges.edges.foldLeft[G[V,E]](onlyNodes)(_ + _)

    graph == newGraph
  }

  // check that we can fully re-create a graph from the contexts
  property("build graph from contexts") = forAll { nodesEdges: GraphNodesEdges[V,E] =>
    val graph = nodesEdges.toGraph
    
    val contexts = graph.ufold[List[Context[V,E]]](_ :: _, List.empty)
    val newGraph = contexts.foldRight(Graph.empty[G,V,E])(_ & _)

    graph == newGraph
  }
}

class SortedMapGraphTests
    extends GraphTests[SortedMapGraph, Short, Short]("SortedMapGraphTests") {

  implicit val impl = SortedMapGraph.graphImpl
  implicit val arbV = Arbitrary.arbShort
  implicit val arbE = Arbitrary.arbShort

}

import java.io.BufferedWriter
import scala.util._
import java.io._
import scala.sys.process._




package object fgl {

  implicit class ContextOps[V,E](context: Context[V,E]) {

    def &[G[_,_]](
      graph: G[V,E]
    )(implicit impl: Graph[G]): G[V,E] =
      impl.combine(context, graph)
  }

  implicit class GraphOps[G[_,_],V,E](graph: G[V,E]) {

    def &(
      context: Context[V, E]
    )(implicit impl: Graph[G]): G[V,E] =
      impl.combine(context, graph)

    def elimAny[A](
      func: Option[(Context[V, E], G[V,E])] => A
    )(implicit impl: Graph[G]): A =
      if (impl.isEmpty(graph)) { func(None) } else { func(Some(impl.matchAny(graph))) }

    def elim[A](node: Node)(
      func: Option[(Context[V, E], G[V,E])] => A
    )(implicit impl: Graph[G]): A = {
      val (optCon, newGraph) = impl.`match`(node, graph)
      func(optCon.map(_ -> newGraph))
    }

    /** Is the graph empty? */
    def isEmpty(implicit impl: Graph[G]): Boolean = elimAny {
      case None => true
      case Some(_) => false
    }

    def range(implicit impl: Graph[G]): (Node, Node) = impl.nodeRange(graph)

    /** Fold a function over the graph by recursively */
    def ufold[C](
      folder: (Context[V, E], C) => C,
      zero: C,
    )(implicit impl: Graph[G]): C = elimAny {
      case None => zero
      case Some((ctx, restGraph)) => folder(ctx, restGraph.ufold(folder, zero))
    }

    /** Map the values, but preserve the structure */
    def map[A,B](
      nodeMap: V => A,
      edgeMap: E => B,
    )(implicit impl: Graph[G]): G[A,B] = gmap(_.map(nodeMap, edgeMap))

    /** Map the graph through contexts */
    def gmap[A,B](
      contextMap: Context[V,E] => Context[A,B]
    )(implicit impl: Graph[G]): G[A,B] = elimAny {
      case None => impl.empty[A,B]
      case Some((ctx, restGraph)) => contextMap(ctx) & restGraph.gmap(contextMap)
    }

    def nodes(implicit impl: Graph[G]): Iterable[LabelledNode[V]] = impl.nodes(graph)

    def edges(implicit impl: Graph[G]): Iterable[LabelledEdge[E]] = impl.edges(graph)

    /** List `n` node _not_ used yet in the graph */
    def newNodes(n: Int)(implicit impl: Graph[G]): Seq[Node] =
      if (isEmpty) {
        (0 until n).map(Node(_))
      } else {
        val upper = 1 + impl.nodeRange(graph)._2.id
        (upper until (n + upper)).map(Node(_))
      }

    def contains(node: Node)(implicit impl: Graph[G]): Boolean = elim(node) {
      case None => false
      case Some(_) => true
    }

    def size(implicit impl: Graph[G]): Int = impl.nodeCount(graph)

    def contains(node: LabelledNode[V])(implicit impl: Graph[G]): Boolean = elim(node.node) {
      case None => false
      case Some((ctx, _)) => ctx.value == node.value
    }

    // TODO: this is slower than it needs to be
    def contains(edge: LabelledEdge[E])(implicit impl: Graph[G]): Boolean = elim(edge.from) {
      case None => false
      case Some((ctx, _)) => ctx.fromNode.edges.contains((edge.label, edge.to))
    }


    /** Find all nodes that link to the given node */
    def pre(node: Node)(implicit impl: Graph[G]): Iterable[Node] = elim(node) {
      case None => Iterable.empty
      case Some((ctx, _)) => ctx.toNode.edges.view.map(_._2)
    }

    /** Find all nodes that link from the given node */
    def suc(node: Node)(implicit impl: Graph[G]): Iterable[Node] = elim(node) {
      case None => Iterable.empty
      case Some((ctx, _)) => ctx.fromNode.edges.view.map(_._2)
    }

    def +(node: LabelledNode[V])(implicit impl: Graph[G]): G[V,E] =
      Context(Adj.empty, node.node, node.value, Adj.empty) & graph

    def +(edge: LabelledEdge[E])(implicit impl: Graph[G]): G[V,E] = elim(edge.from) {
      case None => throw new IllegalArgumentException(s"Cannot add edge from non-existent vertex ${edge.from}")
      case Some((ctx, restGraph)) =>
        val newToNode = if (edge.to == edge.from) {
          Adj((edge.label, edge.from) :: ctx.toNode.edges)
        } else {
          ctx.toNode
        }
        val newCtx = Context(
          fromNode = Adj((edge.label, edge.to) :: ctx.fromNode.edges),
          node = ctx.node,
          value = ctx.value,
          toNode = newToNode,
        )
        newCtx & restGraph
    }

    def -(node: Node)(implicit impl: Graph[G]): G[V,E] = elim(node) {
      case None => graph
      case Some((_, restGraph)) => restGraph
    }

    /** Remove all edges connecting one node to another with that label */
    def -(edge: LabelledEdge[E])(implicit impl: Graph[G]): G[V,E] = elim(edge.from) {
      case None => graph
      case Some((ctx, restGraph)) => 
        val newToNode = if (edge.to == edge.from) {
          Adj(ctx.toNode.edges.filter(_ != (edge.label -> edge.from)))
        } else {
          ctx.toNode
        }
        val newCtx = Context(
          fromNode = Adj(ctx.fromNode.edges.filter(_ != (edge.label -> edge.to))),
          node = ctx.node,
          value = ctx.value,
          toNode = newToNode,
        )
        newCtx & restGraph
    }

    /** Remove all edges connecting one node to another */
    def -(edge: Edge)(implicit impl: Graph[G]): G[V,E] = elim(edge.from) {
      case None => graph
      case Some((ctx, restGraph)) =>
        val newToNode = if (edge.to == edge.from) {
          Adj(ctx.toNode.edges.filter(_._2 != edge.from))
        } else {
          ctx.toNode
        }
        val newCtx = Context(
          fromNode = Adj(ctx.fromNode.edges.filter(_._2 != edge.to)),
          node = ctx.node,
          value = ctx.value,
          toNode = newToNode,
        )
        newCtx & restGraph
    }

    /** Remove one edge connecting one node to another with that label */
    def removeOne(edge: LabelledEdge[E])(implicit impl: Graph[G]): G[V,E] = elim(edge.from) {
      case None => graph
      case Some((ctx, restGraph)) =>
        val newToNode = if (edge.to == edge.from) {
          val (pre, _ :: suf) = ctx.toNode.edges.span(_ != (edge.label -> edge.from))
          Adj(pre ++ suf)
        } else {
          ctx.toNode
        }

        val newCtx = Context(
          fromNode = Adj {
            val (pre, _ :: suf) = ctx.fromNode.edges.span(_ != (edge.label -> edge.to))
            pre ++ suf
          },
          node = ctx.node,
          value = ctx.value,
          toNode = newToNode,
        )
        newCtx & restGraph
    }

    def addAllNodes(nodes: LabelledNode[V]*)(implicit impl: Graph[G]): G[V,E] =
      nodes.foldLeft(graph) { case (graph, node) => graph + node }

    def addAllEdges(edges: LabelledEdge[E]*)(implicit impl: Graph[G]): G[V,E] =
      edges.foldLeft(graph) { case (graph, edge) => graph + edge }

    def removeAllNodes(nodes: Node*)(implicit impl: Graph[G]): G[V,E] =
      nodes.foldLeft(graph) { case (graph, node) => graph - node }

    def removeAllEdges(edges: Edge*)(implicit impl: Graph[G]): G[V,E] =
      edges.foldLeft(graph) { case (graph, edge) => graph - edge }



    def get(node: Node)(implicit impl: Graph[G]): Option[LabelledNode[V]] =
      elim(node) {
        case Some((context,_)) => Some(LabelledNode(context.node, context.value))
        case None => None
      }


    def dotFile(
      name: String,
      printNode: LabelledNode[V] => String = _.value.toString,
      printEdge: LabelledEdge[E] => String = _.label.toString,
    )(implicit impl: Graph[G]): DotFile = {
      val file = new java.io.File(name)

      Using(new BufferedWriter(new FileWriter(file))) { writer =>
        writer.write("digraph {\n")
        for (node <- nodes)
          writer.write(s"  ${node.node.id} [label=" + "\"" + printNode(node) + "\"];\n")
        for (edge <- edges)
          writer.write(s"  ${edge.from.id} -> ${edge.to.id} [label=" + "\"" + printEdge(edge) + "\"];\n")
        writer.write("}\n")
      }

      DotFile(file)
    }
  }


  case class DotFile(regular: java.io.File) {

    def svg(fileName: String): java.io.File = {
      Process("dot", Seq("-Tsvg", regular.getPath(), s"-o$fileName")).! match {
        case 0 => new java.io.File(fileName)
        case _ => throw new Exception("Failed to make dot SVG")
      }
    }
  }

}

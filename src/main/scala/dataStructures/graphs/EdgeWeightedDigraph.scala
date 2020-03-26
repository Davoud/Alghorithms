
package dataStructures.graphs

import scala.collection.mutable
import scala.collection.mutable.HashSet

case class DirectedEdge(from: Int, to: Int, weight: Double) {
	override def toString: String = s"$from->$to $weight"
}


class EdgeWeightedDigraph(val numberOfVertices: Int) {
	private val _adj = Array.fill[Bag](numberOfVertices)(EmptyBag)
	
	def addEdge(edge: DirectedEdge): Unit = _adj(edge.from) += edge
	
	def adj(vertex: Int): Iterable[DirectedEdge] = _adj(vertex)
	
	def edges: Iterable[DirectedEdge] = _adj.fold(EmptyBag)((a, b) => a | b)
	
	def +=(edges: (Int, Int, Double)*): EdgeWeightedDigraph = {
		edges.foreach(e => addEdge(DirectedEdge(e._1, e._2, e._3)))
		this
	}
	
	type Bag = HashSet[DirectedEdge]
	
	private def EmptyBag: Bag = new mutable.HashSet[DirectedEdge]()
}

class Topological(graph: EdgeWeightedDigraph) {
	
	private val marked = Array.fill[Boolean](graph.numberOfVertices)(false)
	private val reversePost = mutable.ArrayStack[Int]()
	
	for (v <- 0 until graph.numberOfVertices)
		if (!marked(v)) dfs(v)
	
	private def dfs(v: Int): Unit = {
		marked(v) = true
		for (edge <- graph.adj(v))
			if (!marked(edge.to)) dfs(edge.to)
		reversePost push v
	}
	
	def order: Iterable[Int] = reversePost
}
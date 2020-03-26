package dataStructures.graphs

import scala.collection.mutable

abstract class ShortestPath(graph: EdgeWeightedDigraph, sourceVertex: Int) {
	
	protected val edgeTo = Array.fill[Option[DirectedEdge]](graph.numberOfVertices)(None)
	protected val distTo = Array.fill[Double](graph.numberOfVertices)(Double.PositiveInfinity)
	distTo(sourceVertex) = 0.0
	
	protected def relax(edge: DirectedEdge): Unit
	
	def pathTo(vertex: Int): Iterable[DirectedEdge] = {
		require(0 <= vertex && vertex < edgeTo.length)
		val path = new mutable.ArrayStack[DirectedEdge]()
		var edge = edgeTo(vertex)
		while (edge.isDefined) {
			path push edge.get
			edge = edgeTo(edge.get.from)
		}
		path
	}
	
	def distanceTo(vertex: Int): Double = {
		require(0 <= vertex && vertex < distTo.length)
		distTo(vertex)
	}
}

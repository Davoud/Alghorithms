package dataStructures.graphs

import dataStructures.heaps.IndexMinPQ
import scala.collection.mutable

class DijkstraShortestPath(graph: EdgeWeightedDigraph, source: Int) {
	
	private val edgeTo = Array.fill[Option[DirectedEdge]](graph.numberOfVertices)(None)
	private val distTo = Array.fill(graph.numberOfVertices)(Double.MaxValue)
	private val pq = new IndexMinPQ[Double](graph.numberOfVertices)
	
	distTo(source) = 0
	pq insert(source, 0.0)
	while (!pq.isEmpty) {
		val v = pq.delMin()
		for (e <- graph adj v)
			relax(e)
	}
	
	private def relax(edge: DirectedEdge): Unit = {
		val s = edge.from
		val t = edge.to
		if (distTo(t) > distTo(s) + edge.weight) {
			distTo(t) = distTo(s) + edge.weight
			edgeTo(t) = Some(edge)
			if (pq contains t) pq decreaseKey(t, distTo(t))
			else pq insert(t, distTo(t))
		}
	}
	
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

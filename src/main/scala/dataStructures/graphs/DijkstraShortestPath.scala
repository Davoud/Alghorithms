package dataStructures.graphs

import dataStructures.heaps.IndexMinPQ

class DijkstraShortestPath(graph: EdgeWeightedDigraph, source: Int)
	extends ShortestPath(graph, source) {
	
	private val pq = new IndexMinPQ[Double](graph.numberOfVertices)
	
	pq insert(source, 0.0)
	while (!pq.isEmpty) {
		val v = pq.delMin()
		for (e <- graph adj v)
			relax(e)
	}
	
	override protected def updated(edge: DirectedEdge): Unit = {
		val t = edge.to
		if (pq contains t) pq decreaseKey(t, distTo(t))
		else pq insert(t, distTo(t))
	}
	
}

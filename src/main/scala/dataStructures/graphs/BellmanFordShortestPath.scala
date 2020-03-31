package dataStructures.graphs

import dataStructures.queues.ArrayQueue

class BellmanFordShortestPath(graph: EdgeWeightedDigraph, source: Int) extends ShortestPath(graph, source) {
	
	val distToChanged = new ArrayQueue[Int]()
	
	
	for (i <- 0 until graph.numberOfVertices) {
		
		for (v <- 0 until graph.numberOfVertices) {
			for (e <- graph.adj(v))
				relax(e)
		}
	}
	
	override protected def updated(edge: DirectedEdge): Unit = {
		if (!distToChanged.contains(edge.to))
			distToChanged.enqueue(edge.to)
	}
	
}

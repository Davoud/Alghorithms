package dataStructures.graphs

import dataStructures.queues.ArrayQueue

class BellmanFordShortestPath(graph: EdgeWeightedDigraph, source: Int) extends ShortestPath(graph, source) {
	
	val distTochanged = new ArrayQueue[Int]()
	
	for {
		i <- 0 until graph.numberOfVertices
		v <- 0 until graph.numberOfVertices
		e <- graph.adj(v)
	} relax(e)
	
	override protected def updated(edge: DirectedEdge): Unit = {
		distTochanged.enqueue(edge.to)
	}
	
}

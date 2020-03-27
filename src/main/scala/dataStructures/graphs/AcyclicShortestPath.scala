package dataStructures.graphs

import scala.collection.mutable

class AcyclicShortestPath(graph: EdgeWeightedDigraph, sourceVertex: Int)
	extends ShortestPath(graph, sourceVertex) {
	require(!new EdgeWeightedDirectedCycle(graph).hasCycle, "The given graph is NOT acyclic!!!")
	
	for (v <- new Topological(graph).order; e <- graph.adj(v))
		relax(e)
	
}

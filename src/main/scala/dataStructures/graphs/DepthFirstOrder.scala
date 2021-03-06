package dataStructures.graphs

import scala.collection.mutable.{ArrayStack, Map}

class DepthFirstOrder[Vertex: Manifest](graph: Graph[Vertex])(implicit o: Ordering[Vertex]) {
	
	private val marked = Map[Vertex, Boolean]()
	private val reversPost = ArrayStack[Vertex]()
	
	for (vertex <- graph.vertices) {
		if (notMarked(vertex))
			dfs(vertex)
	}
	
	private def dfs(vertex: Vertex): Unit = {
		marked += vertex -> true
		
		for (v <- graph.adj(vertex))
			if (notMarked(v)) dfs(v)
		
		reversPost push vertex
	}
	
	private def notMarked(vertex: Vertex): Boolean = !marked.contains(vertex) || !marked(vertex)
	
	def reversePost: Iterable[Vertex] = reversPost
}

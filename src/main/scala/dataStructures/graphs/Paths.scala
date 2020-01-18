package dataStructures.graphs


import scala.collection.mutable.{ListBuffer, Map}

class Paths[Vertex](G: Graph[Vertex], s: Vertex) {
	
	private val marked = Map[Vertex, Boolean]()
	private val edgeTo = Map[Vertex, Option[Vertex]]()
	
	for (vertex <- G.vertices)
		marked += vertex -> false
	
	depthFirstSearch()
	
	private def depthFirstSearch(): Unit = visit(s)
	
	private def visit(v: Vertex): Unit = {
		marked(v) = true
		for (adj <- G.adj(v))
			if (!marked(adj)) {
				visit(adj)
				edgeTo(adj) = Some(v)
			}
		
	}
	
	private def pathTo(v: Vertex, path: ListBuffer[Vertex]): Unit = {
		val edge = edgeTo(v).get
		path += edge
		if (edge != s)
			pathTo(edge, path)
	}
	
	def hasPathTo(v: Vertex): Boolean = marked(v)
	
	def pathTo(v: Vertex): Iterable[Vertex] = {
		val path = ListBuffer[Vertex]()
		if (hasPathTo(v)) {
			path += v
			pathTo(v, path)
		}
		path
	}
}

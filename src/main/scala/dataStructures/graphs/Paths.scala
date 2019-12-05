package dataStructures.graphs

import scala.collection.mutable

class Paths(G: Graph, s: Int) {
	
	private val marked = Array.fill[Boolean](G.vertices)(false)
	private val edgeTo = Array.fill[Option[Int]](G.vertices)(None)
	depthFirst()
	
	private def depthFirst(): Unit = visit(s)
	
	private def visit(v: Int): Unit = {
		marked(v) = true
		for (adj <- G.adj(v))
			if (!marked(adj)) {
				visit(adj)
				edgeTo(adj) = Some(v)
			}
		
	}
	
	private def pathTo(v: Int, path: mutable.ListBuffer[Int]): Unit = {
		val edge = edgeTo(v).get
		path += edge
		if (edge != s)
			pathTo(edge, path)
	}
	
	def hasPathTo(v: Int): Boolean = marked(v)
	
	def pathTo(v: Int): Iterable[Int] = {
		val path = mutable.ListBuffer[Int]()
		if (hasPathTo(v)) {
			path += v
			pathTo(v, path)
		}
		path
	}
}

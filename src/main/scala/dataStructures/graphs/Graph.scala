package dataStructures.graphs

trait Graph {
	def addEdge(v: Int, w: Int): Unit
	
	def adj(v: Int): Iterable[Int]
	
	def vertices: Int
	
	def edges: Int
}


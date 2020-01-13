package dataStructures.graphs

trait Graph[Vertex] {
	def addEdge(v: Vertex, w: Vertex): Unit
	
	def adj(v: Vertex): Iterable[Vertex]
	
	def vertices: Iterable[Vertex]
	
	def edges: Int
	
}


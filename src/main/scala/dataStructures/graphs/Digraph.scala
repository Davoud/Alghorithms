package dataStructures.graphs

import scala.collection.mutable
import scala.collection.mutable.{HashSet, Map}

class Digraph[Vertex](v: Int) extends Graph[Vertex] {
	override def addEdge(v: Vertex, w: Vertex): Unit = ???
	
	override def adj(v: Vertex): Iterable[Vertex] = ???
	
	override def vertices: Iterable[Vertex] = ???
	
	override def edges: Int = ???
}



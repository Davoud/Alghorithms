package dataStructures.graphs

import scala.collection.mutable.{HashSet, Map}

class Digraph[Vertex] extends Graph[Vertex] {
	
	private val graph = Map[Vertex, Option[HashSet[Vertex]]]()
	
	private var count: Int = 0
	
	override def addEdge(v: Vertex, w: Vertex): Unit = {
		this += v
		if (graph(v).get.add(w))
			count += 1
	}
	
	def +=(edges: (Vertex, Vertex)*): Unit = edges.foreach(edge => addEdge(edge._1, edge._2))
	
	override def adj(v: Vertex): Iterable[Vertex] = graph(v).get
	
	def +=(vertex: Vertex): Unit = {
		if (!graph.contains(vertex))
			graph += (vertex -> Some(HashSet[Vertex]()))
	}
	
	override def vertices: Iterable[Vertex] = graph.keys
	
	override def edges: Int = count
}



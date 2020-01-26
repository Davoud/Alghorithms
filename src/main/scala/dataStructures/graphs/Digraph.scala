package dataStructures.graphs

import scala.collection.mutable.{HashSet, Map}

class Digraph[Vertex](implicit o: Ordering[Vertex]) extends Graph[Vertex] {
	
	private val graph = Map[Vertex, HashSet[Vertex]]()
	
	private var count: Int = 0
	
	override def addEdge(v: Vertex, w: Vertex): Unit = {
		this += v
		if (graph(v).add(w)) {
			count += 1
		}
	}
	
	def +=(edges: (Vertex, Vertex)*): Unit = edges.foreach(edge => addEdge(edge._1, edge._2))
	
	override def adj(v: Vertex): Iterable[Vertex] = graph(v)
	
	def +=(vertex: Vertex): Unit = {
		if (!graph.contains(vertex))
			graph += (vertex -> HashSet[Vertex]())
	}
	
	override def vertices: Iterable[Vertex] = graph.keys.toList.sorted(o)
	
	override def edges: Int = count
}



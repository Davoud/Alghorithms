package dataStructures.graphs

import scala.collection.mutable.{HashSet, Map}

class SparseGraph[Vertex](v: Int) extends Graph[Vertex] {
	
	private val graph = Map[Vertex, Option[HashSet[Vertex]]]()
	
	private var count: Int = 0
	
	override def addEdge(v: Vertex, w: Vertex): Unit = {
		if (!graph.contains(v)) graph += (v -> Some(HashSet[Vertex]()))
		if (!graph.contains(w)) graph += (w -> Some(HashSet[Vertex]()))
		if (graph(v).get.add(w) || graph(w).get.add(v)) count += 1
	}
	
	def addEdges(edges: (Vertex, Vertex)*): Unit = edges.foreach(edge => addEdge(edge._1, edge._2))
	
	override def adj(v: Vertex): Iterable[Vertex] = graph(v).get
	
	override def edges: Int = count
	
	override def vertices = graph.keys
}

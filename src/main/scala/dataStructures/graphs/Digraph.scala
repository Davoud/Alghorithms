package dataStructures.graphs

import scala.collection.mutable
import scala.collection.mutable.{HashSet, Map}

class Digraph[Vertex](initialEdges: (Vertex, Vertex)*)(implicit o: Ordering[Vertex]) extends Graph[Vertex] {
	
	private val graph = Map[Vertex, HashSet[Vertex]]()
	
	private var count: Int = 0
	
	initialEdges.foreach(edge => addEdge(edge._1, edge._2))
	
	override def addEdge(v: Vertex, w: Vertex): Unit = {
		this += v
		this += w
		if (graph(v).add(w))
			count += 1
	}
	
	def +=(edges: (Vertex, Vertex)*): Unit = edges.foreach(edge => addEdge(edge._1, edge._2))
	
	override def adj(v: Vertex): Iterable[Vertex] = graph(v)
	
	def +=(vertex: Vertex): Unit = {
		if (!graph.contains(vertex))
			graph += (vertex -> HashSet[Vertex]())
	}
	
	override def vertices: Iterable[Vertex] = graph.keys.toList.sorted(o)
	
	override def edges: Int = count
	
	def size: Int = graph.size
	
	def reversed: Digraph[Vertex] = {
		val reversedDigraph = new Digraph[Vertex]()
		for (v <- vertices; w <- adj(v))
			reversedDigraph += (w -> v)
		reversedDigraph
	}
	
	override def toString(): String = {
		var s = new mutable.StringBuilder()
		for (v <- vertices) {
			val neighbours = adj(v).fold("")((a, s) => s"$a $s")
			s.append(s"$v ->$neighbours | ")
		}
		s.toString()
	}
}



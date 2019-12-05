package dataStructures.graphs

import scala.collection.mutable.HashSet

class SparseGraph(v: Int) extends Graph {
	
	val graph = Array.fill[Option[HashSet[Int]]](v)(None)
	
	var count: Int = 0
	
	override def addEdge(v: Int, w: Int): Unit = {
		if (graph(v).isEmpty) graph(v) = Some(HashSet[Int]())
		if (graph(w).isEmpty) graph(w) = Some(HashSet[Int]())
		if (graph(v).get.add(w) || graph(w).get.add(v)) count += 1
	}
	
	def addEdges(edges: (Int, Int)*): Unit = edges.foreach(edge => addEdge(edge._1, edge._2))
	
	override def adj(v: Int): Iterable[Int] = graph(v).getOrElse(Array[Int]())
	
	override def edges: Int = count
	
	override def vertices: Int = v
}

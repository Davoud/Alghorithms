package dataStructures.graphs

import scala.collection.mutable.Map

class KosarajuSharirSCC[V: Manifest](digraph: Digraph[V])(implicit o: Ordering[V]) {
	
	private case class Status(marked: Boolean = false, id: Int = 0)
	
	private val vertices = Map[V, Status]()
	private var count: Int = 0
	
	for (v <- digraph.vertices)
		vertices += v -> Status()
	
	val ordered = new DepthFirstOrder[V](digraph.reversed)
	
	for (v <- ordered.reversePost)
		if (!vertices(v).marked) {
			dfs(v)
			count += 1
		}
	
	
	private def dfs(v: V): Unit = {
		vertices(v) = Status(true, count)
		for (w <- digraph.adj(v))
			if (!vertices(w).marked)
				dfs(w)
	}
	
	def stronglyConnected(v: V, w: V): Boolean = vertices(v).id == vertices(w).id
	
	override def toString: String = vertices.fold("")((a, s) => s"$a $s |").toString
	
}

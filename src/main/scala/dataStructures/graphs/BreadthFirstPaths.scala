package dataStructures.graphs


import dataStructures.queues.ArrayQueue

import scala.collection.mutable.{ListBuffer, Map, ArrayStack}

class BreadthFirstPaths[Vertex: Manifest](graph: Graph[Vertex], source: Vertex*) {
	private val marked = Map[Vertex, Boolean]()
	private val edgeTo = Map[Vertex, Option[Vertex]]()
	
	for (vertex <- graph.vertices)
		marked += vertex -> false
	
	breadthFirstSearch()
	
	def breadthFirstSearch(): Unit = {
		val queue = new ArrayQueue[Vertex]()
		
		for (s <- source) {
			queue.enqueue(s)
			marked(s) = true
		}
		
		while (!queue.isQueueEmpty) {
			val vertex = queue.dequeue()
			for (v <- graph adj vertex) {
				if (!marked(v)) {
					queue enqueue v
					marked(v) = true
					edgeTo(v) = Some(vertex)
				}
			}
		}
	}
	
	private def pathTo(v: Vertex, path: ListBuffer[Vertex]): Unit = {
		val edge = edgeTo(v).get
		path += edge
		if (!source.contains(edge))
			pathTo(edge, path)
	}
	
	private def pathTo(v: Vertex, path: ArrayStack[Vertex]): Unit = {
		val edge = edgeTo(v).get
		path += edge
		if (!source.contains(edge))
			pathTo(edge, path)
	}
	
	def hasPathTo(v: Vertex): Boolean = marked(v)
	
	def pathTo(v: Vertex): Iterable[Vertex] = {
		val path = ListBuffer[Vertex]()
		if (hasPathTo(v)) {
			path += v
			pathTo(v, path)
		}
		path
	}
	
	def shortestPathTo(v: Vertex): Iterable[Vertex] = {
		val path = ArrayStack[Vertex]()
		if (hasPathTo(v)) {
			path += v
			pathTo(v, path)
		}
		path
	}
	
}

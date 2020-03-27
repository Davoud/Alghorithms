package dataStructures.graphs

class EdgeWeightedDirectedCycle(graph: EdgeWeightedDigraph) {
	private val marked = Array.fill[Boolean](graph.numberOfVertices)(false)
	private val onStack = Array.fill[Boolean](graph.numberOfVertices)(false)
	private val edgeTo = new Array[DirectedEdge](graph.numberOfVertices)
	private var _cycle: Option[List[DirectedEdge]] = None
	
	(0 until graph.numberOfVertices).withFilter(v => !marked(v)).foreach(v => dfs(v))
	
	def cycle(): Option[List[DirectedEdge]] = _cycle
	
	def hasCycle: Boolean = _cycle.isDefined
	
	private def dfs(v: Int): Unit = {
		onStack(v) = true
		marked(v) = true
		graph.adj(v).foreach(e => search(e))
		if (!hasCycle) onStack(v) = false
	}
	
	private def search(e: DirectedEdge): Unit = {
		if (!hasCycle) {
			val w = e.to
			if (!marked(w)) {
				edgeTo(w) = e
				dfs(w)
			} else if (onStack(w)) {
				traceBack(e)
			}
		}
	}
	
	private def traceBack(e: DirectedEdge): Option[List[DirectedEdge]] = {
		_cycle = Some(List[DirectedEdge]())
		
		var x = e
		while (x.from != e.to) {
			_cycle = Some(x :: _cycle.get)
			x = edgeTo(x.from)
		}
		_cycle = Some(x :: _cycle.get)
		
		_cycle
	}
}

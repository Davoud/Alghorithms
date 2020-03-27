package dataStructures

import org.scalatest.{FlatSpec, Matchers}
import dataStructures.graphs.{AcyclicShortestPath, BellmanFordShortestPath, DijkstraShortestPath, DirectedEdge, EdgeWeightedDigraph, EdgeWeightedDirectedCycle, Topological}


object DiSampler {
	
	def sample = digraph
	
	private val n = 8 // (0..7)
	private val digraph: EdgeWeightedDigraph = create2
	
	private def create: EdgeWeightedDigraph = new EdgeWeightedDigraph(n) +=
		((4, 5, 0.35), (5, 4, 0.35), (4, 7, 0.37), (5, 7, 0.28), (7, 5, 0.28), (5, 1, 0.32), (0, 4, 0.38),
			(0, 2, 0.26), (7, 3, 0.39), (1, 3, 0.29), (2, 7, 0.34), (6, 2, 0.40), (3, 6, 0.52), (6, 0, 0.58),
			(6, 4, 0.93))
	
	private def create2: EdgeWeightedDigraph = new EdgeWeightedDigraph(n) +=
		((0, 1, 5.0), (0, 4, 9.0), (0, 7, 8.0), (1, 2, 12.0), (1, 3, 15.0), (1, 7, 4.0), (2, 3, 3.0),
			(2, 6, 11.0), (3, 6, 9.0), (4, 5, 4.0), (4, 6, 20.0), (4, 7, 5.0), (5, 2, 1.0), (5, 6, 13.0),
			(7, 5, 6.0), (7, 2, 7.0))
	
}

class WeightedDigraphsTest extends FlatSpec with Matchers {
	"Sample Digraph" should "have 8 vertecies" in {
		DiSampler.sample.numberOfVertices should be(8)
	}
	"It" should "has 15 edges" in {
		DiSampler.sample.edges.size should be(16)
	}
	"Adjacent vertecies of 0" should "be 1, 7, 4 with weights 5, 8, 9 respectively" in {
		val expected = Set(DirectedEdge(0, 7, 8.0), DirectedEdge(0, 4, 9.0), DirectedEdge(0, 1, 5.0))
		DiSampler.sample.adj(0).toSet == expected should be(true)
	}
	"Topological Order of the Sample Graph" should "be 0, 1, 4, 7, 5, 2, 3, 6" in {
		new Topological(DiSampler.sample).order.toList == List(0, 1, 4, 7, 5, 2, 3, 6) should be(true)
	}
}

class DijkstraSPTest extends FlatSpec with Matchers {
	val sp = new DijkstraShortestPath(DiSampler.sample, 0)
	
	private def pathTo(vertex: Int): List[Int] = sp.pathTo(vertex).map(e => e.from).toList
	
	"Shortest path from 0 to 6 or 3" should "be 0 -> 4 -> 5 -> 2" in {
		pathTo(6) == List(0, 4, 5, 2) should be(true)
		pathTo(3) == List(0, 4, 5, 2) should be(true)
	}
	"Shortest path from 0 to 2" should "be 0 -> 4 -> 5" in {
		pathTo(2) == List(0, 4, 5) should be(true)
	}
	"Shortest path from 0 to 5" should "be 0 -> 4" in {
		pathTo(5) == List(0, 4) should be(true)
	}
	"Shortest path from 0 to 1 or 4 or 7" should "be 0" in {
		pathTo(1) == List(0) should be(true)
		pathTo(4) == List(0) should be(true)
		pathTo(7) == List(0) should be(true)
	}
	"Shortest distance to 6 from 0" should "be 25.0" in {
		sp.distanceTo(6) should be(25.0)
	}
}

class AcyclicSPTest extends FlatSpec with Matchers {
	val sp = new AcyclicShortestPath(DiSampler.sample, 0)
	
	private def pathTo(vertex: Int): List[Int] = sp.pathTo(vertex).map(e => e.from).toList
	
	"Shortest path from 0 to 6 or 3" should "be 0 -> 4 -> 5 -> 2" in {
		pathTo(6) == List(0, 4, 5, 2) should be(true)
		pathTo(3) == List(0, 4, 5, 2) should be(true)
	}
	"Shortest path from 0 to 2" should "be 0 -> 4 -> 5" in {
		pathTo(2) == List(0, 4, 5) should be(true)
	}
	"Shortest path from 0 to 5" should "be 0 -> 4" in {
		pathTo(5) == List(0, 4) should be(true)
	}
	"Shortest path from 0 to 1 or 4 or 7" should "be 0" in {
		pathTo(1) == List(0) should be(true)
		pathTo(4) == List(0) should be(true)
		pathTo(7) == List(0) should be(true)
	}
	"Shortest distance to 6 from 0" should "be 25.0" in {
		sp.distanceTo(6) should be(25.0)
	}
	"AcyclicShortestPath" should "throws IllegalArgumentException if there is a cycle" in {
		var cyclicGraph = new EdgeWeightedDigraph(3) += ((0, 1, 0.0), (1, 2, 0.0), (2, 0, 0.0))
		intercept[IllegalArgumentException] {
			new AcyclicShortestPath(cyclicGraph, 0)
		}
	}
}

class BellmanFordSPTest extends FlatSpec with Matchers {
	val sp = new BellmanFordShortestPath(DiSampler.sample, 0)
	
	private def pathTo(vertex: Int): List[Int] = sp.pathTo(vertex).map(e => e.from).toList
	
	"Shortest path from 0 to 6 or 3" should "be 0 -> 4 -> 5 -> 2" in {
		pathTo(6) == List(0, 4, 5, 2) should be(true)
		pathTo(3) == List(0, 4, 5, 2) should be(true)
	}
	"Shortest path from 0 to 2" should "be 0 -> 4 -> 5" in {
		pathTo(2) == List(0, 4, 5) should be(true)
	}
	"Shortest path from 0 to 5" should "be 0 -> 4" in {
		pathTo(5) == List(0, 4) should be(true)
	}
	"Shortest path from 0 to 1 or 4 or 7" should "be 0" in {
		pathTo(1) == List(0) should be(true)
		pathTo(4) == List(0) should be(true)
		pathTo(7) == List(0) should be(true)
	}
	"Shortest distance to 6 from 0" should "be 25.0" in {
		sp.distanceTo(6) should be(25.0)
	}
}

class EdgeWeightedDirectedCycleTest extends FlatSpec with Matchers {
	"Sample Digraph" should "not have any cycle" in {
		val cycle = new EdgeWeightedDirectedCycle(DiSampler.sample)
		cycle.hasCycle should be(false)
	}
	"Sample Digraph with extra edge" should "have a cycle" in {
		val cycle = new EdgeWeightedDirectedCycle(DiSampler.sample += ((3, 4, 5.5)))
		cycle.hasCycle should be(true)
	}
	"This graph" should "have a cycle as 0 -> 1 -> 2 -> 3 -> 4 -> 0" in {
		val c = new EdgeWeightedDirectedCycle(
			new EdgeWeightedDigraph(5)
				+= ((0, 1, 6.0), (1, 2, 1.0), (2, 3, 2.0), (3, 4, 3.0), (4, 0, 4.0))
		)
		c.hasCycle should be(true)
		c.cycle().get.map(e => e.from) == Seq(0, 1, 2, 3, 4) should be(true)
	}
	"This graph" should "have a cycle as 0 -> 1 -> 2 -> 0" in {
		val c = new EdgeWeightedDirectedCycle(
			new EdgeWeightedDigraph(5)
				+= ((0, 1, 6.0), (1, 2, 1.0), (2, 0, 2.0), (1, 4, 3.0), (2, 3, 4.0)))
		c.hasCycle should be(true)
		c.cycle().get.map(e => e.from) == Seq(0, 1, 2) should be(true)
	}
}


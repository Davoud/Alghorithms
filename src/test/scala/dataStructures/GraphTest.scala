package dataStructures

import dataStructures.graphs.{BreadthFirstPaths, Digraph, Graph, Paths, SparseGraph, DepthFirstOrder}
import org.scalatest.{FlatSpec, Matchers}

object Sampler {
	def graph(): Graph[Int] = {
		val g = new SparseGraph[Int](13)
		g.addEdges(
			(0, 5), (4, 3), (0, 1), (9, 12), (6, 4), (5, 4), (0, 2),
			(11, 12), (9, 10), (0, 6), (7, 8), (9, 11), (5, 3))
		g
	}
	
	def digraph(): Digraph[Int] = {
		val g = new Digraph[Int]()
		g += 1
		g +=
			(0 -> 1, 0 -> 5,
				2 -> 1, 2 -> 3,
				3 -> 2, 3 -> 5,
				4 -> 2, 4 -> 3,
				5 -> 4,
				6 -> 0, 6 -> 4, 6 -> 8, 6 -> 9,
				7 -> 6, 7 -> 9,
				8 -> 6,
				9 -> 10, 9 -> 11,
				10 -> 12,
				11 -> 4, 11 -> 12,
				12 -> 9)
		g
	}
	
	def dag(): Digraph[Int] = {
		val g = new Digraph[Int]()
		
		g +=
			(0 -> 5, 0 -> 2, 0 -> 1,
				3 -> 6, 3 -> 5, 3 -> 4, 3 -> 2,
				6 -> 4, 6 -> 0,
				5 -> 2,
				1 -> 4)
		
		g += 2
		g += 4
		
		g
	}
	
}

class DigraphTest extends FlatSpec with Matchers {
	def sample = Sampler.digraph()
	
	"Digraph" should "has 13 vertices" in {
		sample.vertices.size should be(13)
	}
	
	"It" should "have edges" in {
		sample.edges should be(22)
	}
	
	"It" should "report all neighbours correctly" in {
		sample.adj(0).toSet should be(Set(1, 5))
		sample.adj(1).toSet should be(Set())
		sample.adj(2).toSet should be(Set(1, 3))
	}
	
}

class GraphTest extends FlatSpec with Matchers {
	
	def sampleGraph() = Sampler.graph()
	
	"Graph" should "have 13 vertices" in {
		sampleGraph().vertices should be(13)
	}
	
	"It" should "have 10 edges" in {
		sampleGraph().edges should be(13)
	}
	
	"It" should "report (1,2,5,6) as neighbours of 0" in {
		sampleGraph().adj(0).toSet should be(Set(1, 2, 5, 6))
	}
	
}

class PathsTest extends FlatSpec with Matchers {
	def samplePaths() = new Paths[Int](Sampler.graph(), 0)
	
	"Paths" should "verify paths from vertex 0 to  vertices 1, 2, 3, 4, 5 and 6" in {
		val paths = samplePaths()
		for (v <- 1 to 6)
			paths.hasPathTo(v) should be(true)
		
		for (v <- 7 to 12)
			paths.hasPathTo(v) should be(false)
	}
	
	"It" should "reflect the path to 3 as (5,4,6,0)" in {
		samplePaths().pathTo(3).toSeq should be(Seq(3, 5, 0))
	}
}

class DigraphPathsTest extends FlatSpec with Matchers {
	def samplePaths() = new Paths[Int](Sampler.digraph(), 0)
	
	"Reachable vertices form 0 to 2" should "be (5, 4, 3, 2)" in {
		val path = samplePaths();
		path.pathTo(2).toSeq should be(Seq(2, 4, 5, 0))
	}
	
	"Reachable vertices from 0" should "be 1, 2, 3, 4, 6" in {
		var path = samplePaths();
		for (v <- Seq(1, 2, 3, 4, 5))
			path.hasPathTo(v) should be(true)
	}
	
	"Vertices 6 to 12" should "not be reachable form 0" in {
		var path = samplePaths()
		for (v <- 6 to 12)
			path.hasPathTo(v) should be(false)
	}
}

class BreadthFirstPathsTest extends FlatSpec with Matchers {
	def samplePaths() = new BreadthFirstPaths[Int](Sampler.digraph(), 0)
	
	def multiPath() = new BreadthFirstPaths[Int](Sampler.digraph(), 1, 7, 10)
	
	"Shortest path from 0 to 4" should "be 0 -> 5 -> 4" in {
		val path = samplePaths()
		path.shortestPathTo(4).toSeq should be(Seq(0, 5, 4))
	}
	
	"Shortest path from 1, 7, 10 to 4" should "be 7 -> 6 -> 4" in {
		val path = multiPath()
		path.shortestPathTo(4).toSeq should be(Seq(7, 6, 4))
	}
	
	"Shortest path from 1, 7, 10 to 5" should "be 7 -> 6 -> 0 -> 5" in {
		val path = multiPath()
		path.shortestPathTo(5).toSeq should be(Seq(7, 6, 0, 5))
	}
	
	"Shortest path from 1, 7, 10 to 10" should "be 10 -> 12" in {
		val path = multiPath()
		path.shortestPathTo(12).toSeq should be(Seq(10, 12))
	}
	
}

class DepthFirstOrderTest extends FlatSpec with Matchers {
	"Topological Order of graph" should "be as expected" in {
		val topologicalSorter = new DepthFirstOrder[Int](Sampler.dag())
		topologicalSorter.reversePost.toSeq should be(Seq(3, 6, 0, 5, 2, 1, 4))
		
	}
}
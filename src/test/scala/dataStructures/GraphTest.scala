package dataStructures

import dataStructures.graphs.{Digraph, Graph, Paths, SparseGraph}

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
				7 -> 6, 6 -> 9,
				8 -> 6,
				9 -> 10, 9 -> 11,
				10 -> 12,
				11 -> 4, 11 -> 12,
				12 -> 9)
		g
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
		//sample.adj(1).toSet should be(Set())
		sample.adj(2).toSet should be(Set(1, 3))
	}
}
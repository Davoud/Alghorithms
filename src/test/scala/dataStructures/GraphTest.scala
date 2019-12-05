package dataStructures

import dataStructures.graphs.{Graph, SparseGraph, Paths}
import org.scalatest.{FlatSpec, Matchers}

object Sampler {
	def graph(): Graph = {
		val g = new SparseGraph(13)
		g.addEdges(
			(0, 5), (4, 3), (0, 1), (9, 12), (6, 4), (5, 4), (0, 2),
			(11, 12), (9, 10), (0, 6), (7, 8), (9, 11), (5, 3))
		g
	}
}

class GraphTest extends FlatSpec with Matchers {
	
	def sampleGraph(): Graph = Sampler.graph()
	
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
	def samplePaths(): Paths = new Paths(Sampler.graph(), 0)
	
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

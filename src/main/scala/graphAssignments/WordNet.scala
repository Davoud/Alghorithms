package graphAssignments

import dataStructures.graphs.Digraph
import graphAssignments.IOHelper._

import scala.collection.mutable
import scala.util.Try

object IOHelper {
	def using[A <: {def close(): Unit}, B](resource: A)(f: A => B): B =
		try {
			f(resource)
		} finally {
			resource.close()
		}
	
	def linesFrom(fileName: String): Try[List[String]] = Try {
		using(io.Source.fromFile(fileName)) { source => (for (line <- source.getLines) yield line).toList }
	}
	
	def toDigraph(fileName: String, separator: Char = ','): Digraph[Int] = {
		val graph = new Digraph[Int]()
		val lines = linesFrom(fileName)
		if (lines.isSuccess) {
			for (line <- lines.get) {
				val h = line.split(separator).map(a => a.toInt)
				for (i <- 1 until h.length)
					graph.addEdge(h(0), h(i))
			}
		}
		graph
	}
}


class Synsets(sourceFile: String) extends Iterable[String] {
	
	private var values: Option[Map[String, Int]] = None
	
	val lines: Try[List[String]] = linesFrom(sourceFile)
	
	if (lines.isSuccess) {
		values = Some(lines.get.map(parse).toMap)
	}
	else if (lines.isFailure) {
		println(lines.failed)
	}
	
	private def parse(line: String): (String, Int) = {
		val words = line.split(',')
		(words(1).split(' ')(0), words(0).toInt)
	}
	
	def apply(synonym: String): Int = if (values.isDefined) values.get(synonym) else -1
	
	def apply(index: Int): String = {
		if (values.isEmpty) return ""
		for (value <- values.get)
			if (value._2 == index)
				return value._1
		""
	}
	
	def length: Int = if (values.isDefined) values.get.size else 0
	
	def contains(noun: String): Boolean = values.isDefined && values.contains(noun)
	
	override def iterator: Iterator[String] = values.get.keys.iterator
}

class WordNet(synset: String, hypernyms: String) {
	
	private val synsets = new Synsets(synset)
	private val graph = toDigraph(hypernyms)
	private val sap = new SAP(graph)
	
	def nouns: Iterable[String] = synsets
	
	def isNone(word: String): Boolean = word.indexOf(' ') == -1 && synsets.contains(word)
	
	def distance(nounA: String, nounB: String): Int = ???
	
	def shortestAncestralPath(nounA: String, nounB: String): String =
		synsets(sap.ancestor(synsets(nounA), synsets(nounB)))
	
}


class SAP(G: Digraph[Int]) {
	def length(v: Int, w: Int): Int = ???
	
	def ancestor(v: Int, w: Int): Int = {
		val ansV = new Dfs(G, v)
		val ansW = new Dfs(G, w)
		???
	}
	
	def length(v: Iterable[Int], w: Iterable[Int]) = ???
	
	def ancestor(v: Iterable[Int], w: Iterable[Int]) = ???
	
	class Dfs(G: Digraph[Int], v: Int) {
		private val dist = new mutable.HashMap[Int, Int]()
		
		private def search(v: Int, distance: Int): Unit = {
			
			if (dist.contains(v)) {
				if (dist(v) > distance) {
					dist(v) = distance
				}
			}
			else {
				dist(v) = distance
			}
			
			for (w <- G.adj(v))
				search(w, distance + 1)
		}
		
		def distanceTo(w: Int): Int = dist.getOrElse(w, -1)
	}
	
}




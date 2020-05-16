package graphAssignments

import scala.collection.mutable.{HashSet, Map}
import scala.util.{Try, Success, Failure}


object Control {
	def using[A <: {def close(): Unit}, B](resource: A)(f: A => B): B =
		try {
			f(resource)
		} finally {
			resource.close()
		}
}

class WordNet(synset: String, hypernyms: String) {
	
	def nouns: Iterable[String] = ???
	
	def isNone(word: String): Boolean = ???
	
	def distance(nounA: String, nounB: String): Int = ???
	
	def shortestAncestralPath(nounA: String, nounB: String): String = ???
	
}

class Synsets(sourceFile: String) {
	
	private val values = Map[String, Int]()
	
	private val lines: Try[List[String]] = Try {
		Control.using(io.Source.fromFile(sourceFile)) { source => (for (line <- source.getLines) yield line).toList }
	}
	
	if (lines.isSuccess) {
		for (line <- lines.get) {
			val words = line.split(',')
			values(words(1)) = words(0).toInt
		}
	}
	else if (lines.isFailure) {
		println(lines.failed)
	}
	
	def apply(synonym: String): Int = values(synonym)
	
	def length: Int = values.size
	
}


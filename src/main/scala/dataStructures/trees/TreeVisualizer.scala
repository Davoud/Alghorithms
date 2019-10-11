package dataStructures.trees

import dataStructures.stacks.{ArrayListStack, LinkedListStack}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class TreeVisualizer[Key, Value](tree: Bst[Key, Value]) {
	
	val dash: Char = '\u2500'
	val leftCorner: Char = '\u250C'
	val rightCorner: Char = '\u2510'
	
	def print: Unit = {
		for (line <- lines)
			println(line)
	}
	
	def lines: Iterable[String] = buildTreeImage.compactedImage
	
	private def buildTreeImage: TreeImage = {
		var levelSize = 1
		var level = 0
		var padLen = Math.pow(2, tree.depth()) - 1
		val nodes = tree.nodes()
		val treeImage = new TreeImage()
		
		while (padLen >= 1) {
			val line: StringBuilder = new StringBuilder
			for (i <- 0 until levelSize) {
				val node = nodes.get((level, i))
				if (node.isDefined)
					line.append(pad(node.get.value, padLen.toInt, node.get.left, node.get.right)).append(" ")
				else
					line.append(pad(" ", padLen.toInt, ChildInfo.None, ChildInfo.None)).append(" ")
			}
			treeImage.add(line.toString())
			levelSize *= 2
			padLen /= 2
			level += 1
		}
		
		treeImage
	}
	
	def spaces(len: Int): String = {
		val s = new mutable.StringBuilder()
		for (_ <- 0 until len)
			s.append(" ")
		s.toString()
	}
	
	private def pad(value: String, len: Int, left: ChildInfo.Value, right: ChildInfo.Value): String = {
		if (value == " ")
			return spaces(len)
		
		var halfLength = (len - value.length) / 2
		
		val padLeft = spacesLeft(halfLength, left)
		val padRight = spacesRight(halfLength, right)
		s"$padLeft$value$padRight"
	}
	
	def spacesRight(halfLength: Int, child: ChildInfo.Value): String = {
		
		if (halfLength == 0) return ""
		
		if (child == ChildInfo.None)
			return spaces(halfLength)
		
		val h = halfLength / 2
		val str = new mutable.StringBuilder()
		for (_ <- 0 until h) str.append(dash)
		str.append(rightCorner)
		for (_ <- 0 until h) str.append(" ")
		str.toString()
	}
	
	private def spacesLeft(halfLength: Int, child: ChildInfo.Value): String = {
		
		if (halfLength == 0) return ""
		
		if (child == ChildInfo.None)
			return spaces(halfLength)
		
		val h = halfLength / 2
		
		val str = new mutable.StringBuilder()
		for (_ <- 0 until h) str.append(" ")
		str.append(leftCorner)
		for (_ <- 0 until h) str.append(dash)
		str.toString()
	}
	
	class TreeImage {
		
		private val _lines = new ListBuffer[String]()
		
		def add(line: String): Unit = _lines.append(line)
		
		def lines: Iterable[String] = _lines.toList
		
		def maxLength(lines: Iterable[String]): Int = {
			var max = 0
			for (line <- lines)
				if (line.length > max)
					max = line.length
			max
		}
		
		def compactedImage: Iterable[String] = {
			val width = maxLength(_lines)
			val stack = new LinkedListStack[String]
			
			for (i <- 0 until width) {
				val col = getColumn(i)
				if (stack.isEmpty || stack.top != col)
					stack.push(col)
			}
			
			val compWidth = maxLength(stack)
			val compImage = new ListBuffer[StringBuilder]
			for (_ <- 0 until compWidth)
				compImage.append(new StringBuilder)
			
			for (item <- stack) {
				for (i <- 0 until item.length)
					compImage(i).append(item(i))
			}
			
			compImage.map(s => s.toString()).filter(i => !i.isBlank)
		}
		
		private def getColumn(index: Int): String = {
			val col = new StringBuilder()
			for (line <- _lines)
				col.append(line(index))
			col.toString()
		}
	}
}
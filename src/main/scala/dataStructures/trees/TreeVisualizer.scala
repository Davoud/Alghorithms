package dataStructures.trees

import scala.collection.mutable


class TreeVisualizer[Key, Value](tree: Bst[Key, Value]) {
	
	val dash: Char = '\u2500'
	val leftCorner: Char = '\u250C'
	val rightCorner: Char = '\u2510'
	
	def print(): Unit = {
		
		var levelSize = 1
		var level = 0
		var padLen = Math.pow(2, tree.depth()) - 1
		val nodes = tree.nodes()
		
		while (padLen >= 1) {
			val line: StringBuilder = new StringBuilder
			for (i <- 0 until levelSize) {
				val node = nodes.get((level, i))
				if (node.isDefined)
					line.append(pad(node.get.value, padLen.toInt, node.get.left, node.get.right)).append(" ")
				else
					line.append(pad(" ", padLen.toInt, ChildInfo.None, ChildInfo.None)).append(" ")
			}
			println(line)
			levelSize *= 2
			padLen /= 2
			level += 1
		}
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
}
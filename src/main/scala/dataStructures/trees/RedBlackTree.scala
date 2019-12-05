package dataStructures.trees

import scala.collection.mutable

class RedBlackTree[Key, Value](implicit ordering: Ordering[Key]) extends NodeProvider[Key] {
	
	private var root: Option[Node] = None
	
	
	override def nodes(key: Option[Key]): NodesInfo = {
		val map: NodesInfo = mutable.Map()
		
		if (key.isDefined) {
			val node = getNode(key.get)
			if (node.isDefined)
				fill(map, 0, "0", node)
		}
		else
			fill(map, 0, "0", root)
		
		map
	}
	
	override def depth(key: Option[Key]): Int = {
		if (key.isDefined) {
			val node = getNode(key.get)
			if (node.isDefined) node.get.count else 0
		}
		else if (root.isDefined) root.get.count else 0
	}
	
	def size: Int = size(root)
	
	def get(key: Key): Option[Value] = {
		val node = getNode(key)
		if (node.isDefined) Some(node.get.value) else None
	}
	
	private def getNode(key: Key): Option[Node] = {
		var x: Option[Node] = root
		while (x.isDefined) {
			val cmp = ordering.compare(key, x.get.key)
			if (cmp < 0) x = x.get.left
			else if (cmp > 0) x = x.get.right
			else return x
		}
		None
	}
	
	private def fill(map: NodesInfo, level: Int, identifier: String, node: Option[Node]): Unit = {
		if (node.isEmpty) return
		
		val n = node.get
		
		map((level, Integer.parseInt(identifier, 2))) =
			NodeInfo(n.value.toString, childInfo(n.left), childInfo(n.right))
		
		if (n.left.isDefined)
			fill(map, level + 1, identifier + "0", n.left)
		
		if (n.right.isDefined)
			fill(map, level + 1, identifier + "1", n.right)
	}
	
	@inline private def childInfo(node: Option[Node]): ChildInfo.Value =
		if (node.isEmpty) ChildInfo.None
		else if (node.get.color == NodeColor.Red) ChildInfo.Red
		else ChildInfo.Black
	
	private case class Node(key: Key, var value: Value,
	                        var color: NodeColor.Value,
	                        var left: Option[Node] = None, var right: Option[Node] = None, var count: Int = 0)
	
	def isRed(node: Option[Node]): Boolean = node.isDefined && node.get.color == NodeColor.Red
	
	private def rotateLeft(h: Node): Node = {
		val x = h.right.get
		h.right = x.left
		x.left = Some(h)
		x.color = h.color
		h.color = NodeColor.Red
		x.count = h.count
		h.count = 1 + size(h.left) + size(h.right)
		x
	}
	
	private def rotateRight(h: Node): Node = {
		val x = h.left.get
		h.left = x.right
		x.right = Some(h)
		x.color = h.color
		h.color = NodeColor.Red
		x.count = h.count
		h.count = 1 + size(h.left) + size(h.right)
		x
	}
	
	private def flipColors(h: Node): Unit = {
		h.color = NodeColor.Red
		h.left.get.color = NodeColor.Black
		h.right.get.color = NodeColor.Black
	}
	
	def put(key: Key, value: Value): Unit = {
		root = put(root, key, value)
		root.get.color = NodeColor.Black
	}
	
	private def put(x: Option[Node], key: Key, value: Value): Option[Node] = {
		if (x.isEmpty) return Some(Node(key, value, color = NodeColor.Red, count = 1))
		
		var node = x.get
		
		val cmp = ordering.compare(key, node.key)
		if (cmp < 0) node.left = put(node.left, key, value)
		else if (cmp > 0) node.right = put(node.right, key, value)
		else node.value = value
		
		node.count = 1 + size(node.left) + size(node.right)
		
		if (isRed(node.right) && !isRed(node.left))
			node = rotateLeft(node)
		
		if (isRed(node.left) && (node.left.isDefined && isRed(node.left.get.left)))
			node = rotateRight(node)
		
		if (isRed(node.left) && isRed(node.right))
			flipColors(node)
		
		Some(node)
	}
	
	private def size(x: Option[Node]): Int = if (x.isDefined) x.get.count else 0
	
	object NodeColor extends Enumeration {
		val Red = Value(1)
		val Black = Value(2)
	}
	
}

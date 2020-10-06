package strings

import scala.collection.mutable


class TriesST[Value: Manifest] {
	
	type StringQ = mutable.Queue[String]
	
	private val R = 256
	private var root: Option[Node] = None
	
	case class Node(var value: Option[Value] = None) {
		val next: Array[Option[Node]] = Array.fill(R)(None)
	}
	
	def get(key: String): Option[Value] = get(root, key, 0) match {
		case Some(x) => x.value
		case None => None
	}
	
	def put(key: String, value: Value): Unit = root = put(root, key, value, 0)
	
	def size: Int = size(root)
	
	def keys(): Iterable[String] = keysWithPrefix("")
	
	def keysWithPrefix(pre: String): Iterable[String] = {
		val q = new mutable.Queue[String]()
		collect(get(root, pre, 0), pre, q)
		q
	}
	
	def keysThatMatch(pattern: String): Iterable[String] = {
		val q = new mutable.Queue[String]()
		collect(root, "", pattern, q)
		q
	}
	
	def longestPrefixOf(s: String): String = s.substring(0, search(root, s, 0, 0))
	
	def remove(key: String): Unit = root = delete(root, key, 0)
	
	
	private def delete(x: Option[Node], key: String, d: Int): Option[Node] = {
		if (x.isEmpty) return None
		if (d == key.length) {
			x.get.value = None
		}
		else {
			x.get.next(key(d)) = delete(x.get.next(key(d)), key, d + 1)
		}
		
		if (x.get.value.isDefined) return x
		
		for (c <- 0 until R)
			if (x.get.next(c) != null)
				return x
		
		None
		
	}
	
	
	private def search(x: Option[Node], s: String, d: Int, length: Int): Int = {
		if (x.isEmpty) return length
		val len = if (x.get.value.isDefined) d else length
		if (d == s.length) return len
		search(x.get.next(s(d)), s, d + 1, len)
	}
	
	private def collect(x: Option[Node], pre: String, pattern: String, q: StringQ): Unit = {
		if (x.isEmpty) return
		val d = pre.length
		if (d == pattern.length && x.get.value.isDefined) q enqueue pre
		if (d == pattern.length) return
		val next = pattern(d)
		for (c <- 0 until R)
			if (next == '.' || next == c)
				collect(x.get.next(c), pre + c.toChar, pattern, q)
	}
	
	private def collect(x: Option[Node], pre: String, q: StringQ): Unit = {
		if (x.isEmpty) return
		if (x.get.value.isDefined) q enqueue pre
		for (c <- 0 until R)
			collect(x.get.next(c), pre + c.toChar, q)
	}
	
	private def put(x: Option[Node], key: String, value: Value, d: Int): Option[Node] = {
		val y = if (x.isDefined) x.get else Node()
		if (d == key.length) {
			y.value = Some(value)
			return Some(y)
		}
		val c = key(d)
		y.next(c) = put(y.next(c), key, value, d + 1)
		Some(y)
	}
	
	private def get(x: Option[Node], key: String, d: Int): Option[Node] = x match {
		case None => None
		case Some(y) => key.length match {
			case len if (len == d) => Some(y)
			case _ => get(y.next(key(d)), key, d + 1)
		}
	}
	
	private def size(x: Option[Node]): Int = {
		if (x.isEmpty) return 0
		var count = 0
		if (x.get.value.isDefined) count += 1
		for (c <- 0 until R)
			count += size(x.get.next(c))
		count
	}
	
	
	@inline def update(key: String, value: Value): Unit = put(key, value)
	
	@inline def apply(key: String): Option[Value] = get(key)
}

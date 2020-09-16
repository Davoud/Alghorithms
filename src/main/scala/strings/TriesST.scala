package strings


class TriesST[Value: Manifest] {
	
	private val R = 256
	private var root: Option[Node] = None
	
	case class Node(var value: Option[Value] = None) {
		val next = new Array[Node](R)
	}
	
	def get(key: String): Option[Value] = get(root, key, 0) match {
		case Some(x) => x.value
		case None => None
	}
	
	def put(key: String, value: Value): Unit = root = Option(put(root, key, value, 0))
	
	def put(x: Option[Node], key: String, value: Value, d: Int): Node = {
		val y = if (x.isDefined) x.get else Node()
		if (d == key.length) {
			y.value = Some(value)
			return y
		}
		val c = key(d)
		y.next(c) = put(Option(y.next(c)), key, value, d + 1)
		y
	}
	
	private def get(x: Option[Node], key: String, d: Int): Option[Node] = x match {
		case None => None
		case Some(y) => key.length match {
			case len if (len == d) => Some(y)
			case _ => get(Option(y.next(key(d))), key, d + 1)
		}
	}
	
	@inline def update(key: String, value: Value): Unit = put(key, value)
	
	@inline def apply(key: String): Option[Value] = get(key)
}

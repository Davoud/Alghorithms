package dataStructures.stacks

import scala.collection.mutable.ListBuffer


class LinkedListStack[T] extends Stack[T] {
	private var first: Node[T] = _

	
	override def isEmpty: Boolean = first == null
	
	override def push(item: T): Unit = {
		val oldFirst = first;
		first = Node(item, oldFirst)
	}
	
	override def pop(): T = {
		if (!isEmpty) {
			val item = first.value
			first = first.next
			return item
		}
		throw new Exception("Stack is Empty")
	}
	
	case class Node[A](value: A, next: Node[A])
	
	override def top: T = first.value
	
	private def items: List[T] = {
		val list = new ListBuffer[T]
		var node = first
		while (node != null) {
			list.append(node.value)
			node = node.next
		}
		list.toList.reverse
	}
	
	override def iterator: Iterator[T] = items.iterator
}

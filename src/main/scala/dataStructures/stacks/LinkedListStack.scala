package dataStructures.stacks


class LinkedListStack[T] extends Stack[T] {
	private var first: Node[T] = _
	private var size: Int = 0
	
	override def isEmpty: Boolean = first == null
	
	override def push(item: T): Unit = {
		var oldFirst = first;
		first = Node(item, oldFirst)
	}
	
	override def pop(): T = {
		if (!isEmpty) {
			var item = first.value
			first = first.next
			return item
		}
		throw new Exception("Stack is Empty")
	}
	
	case class Node[A](value: A, next: Node[A])
	
}

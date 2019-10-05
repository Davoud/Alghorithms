package dataStructures.queues

import scala.annotation.tailrec


class LinkedListQueue[A: Manifest] extends Queue[A] {
	
	private var firstItem: Option[Node[A]] = None
	private var lastItem: Option[Node[A]] = None
	
	def enqueue(item: A): Unit = {
		val oldLast = lastItem
		lastItem = Option(Node[A](item))
		if (isQueueEmpty)
			firstItem = lastItem
		else
			oldLast.get.next = lastItem
	}
	
	def dequeue(): A = {
		if (isQueueEmpty) throw new Exception("Queue is empty")
		val item = firstItem.get.value
		firstItem = firstItem.get.next
		item
	}
	
	def isQueueEmpty: Boolean = firstItem.isEmpty
	
	override def toString: String = {
		travers(firstItem, "")
	}
	
	@tailrec private def travers(node: Option[Node[A]], q: String): String = {
		if (node.isDefined)
			travers(node.get.next, s"$q ${node.get.value}")
		else
			q
	}
	
	case class Node[T](var value: T, var next: Option[Node[T]] = None)
	
	
	override def iterator: Iterator[A] = new QueueIterator[A](firstItem)
	
	
	class QueueIterator[T](var node: Option[Node[T]]) extends Iterator[T] {
		override def hasNext: Boolean = node.isDefined
		
		override def next(): T = {
			val item = node.get.value
			node = node.get.next
			item
		}
	}
	
	def shuffle(): Unit = {
		val q = new RandomizedQueue[A](4)
		
		while (!isQueueEmpty)
			q enqueue dequeue
		
		while (!q.isQueueEmpty)
			enqueue(q dequeue)
		
	}
}

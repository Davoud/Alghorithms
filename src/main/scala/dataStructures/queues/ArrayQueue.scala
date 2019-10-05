package dataStructures.queues

class ArrayQueue[A: Manifest](capacity: Int = 4) extends Queue[A] {
	var queue = new Array[Option[A]](capacity)
	var headIndex: Int = 0
	var tailIndex: Int = 0
	var qSize: Int = 0
	
	fillWithEmptyElement(queue)
	
	override def enqueue(item: A): Unit = {
		if (isFull) resize(queueLength * 2)
		queue(tailIndex) = Some(item)
		tailIndex = (tailIndex + 1) % queueLength
		qSize += 1
	}
	
	override def dequeue(): A = {
		if (isQueueEmpty)
			throw new Exception("Queue is empty")
		
		val item = queue(headIndex)
		queue(headIndex) = None
		headIndex = (headIndex + 1) % queueLength
		qSize -= 1
		if (shrinkable()) resize(queueLength / 2)
		item.get
	}
	
	override def isQueueEmpty: Boolean = qSize == 0
	
	private def shrinkable(): Boolean = qSize < queueLength / 4 && queueLength > capacity
	
	def queueLength(): Int = queue.length
	
	private def isFull: Boolean = queueLength() == qSize
	
	private def fillWithEmptyElement(array: Array[Option[A]]): Unit = {
		for (i <- array.indices)
			array(i) = None
	}
	
	def queueSize(): Int = qSize
	
	private def resize(newLength: Int): Unit = {
		val newQueue = new Array[Option[A]](newLength)
		fillWithEmptyElement(newQueue)
		var index: Int = -1
		for (i <- 0 until qSize) {
			index += 1
			newQueue(index) = queue((i + headIndex) % queueLength)
		}
		queue = newQueue
		headIndex = 0
		tailIndex = index + 1
	}
	
	override def toString(): String = {
		var s = ""
		for (i <- 0 until queueLength()) {
			val item = queue(i)
			s += (if (item.isEmpty) "_ " else queue(i) + " ")
		}
		s
	}
	
	override def iterator: Iterator[A] = new QueueIterator[A](queue)
	
	class QueueIterator[T](q: Array[Option[T]]) extends Iterator[T] {
		var index: Int = 0
		
		override def hasNext: Boolean = index < qSize
		
		override def next(): T = {
			val item = q((headIndex + index) % q.length)
			index += 1
			item.get
		}
	}
	
}
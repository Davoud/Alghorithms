package dataStructures

import scala.annotation.tailrec

trait Queue[A] extends Iterable[A]
{
    def enqueue(item: A)
    def dequeue(): A
    def isQueueEmpty: Boolean
}

class LinkedListQueue[A] extends Queue[A] {

    private var firstItem: Option[Node[A]] = None
    private var lastItem: Option[Node[A]] = None

    def enqueue(item: A): Unit = {
        val oldLast = lastItem
        lastItem = Option(Node[A](item))
        if(isQueueEmpty)
            firstItem = lastItem
        else
            oldLast.get.next = lastItem
    }

    def dequeue(): A = {
        if(isQueueEmpty) throw new Exception("Queue is empty")
        val item = firstItem.get.value
        firstItem = firstItem.get.next
        item
    }

    def isQueueEmpty: Boolean = firstItem.isEmpty

    override def toString: String = {
        travers(firstItem, "")
    }

    @tailrec private def travers(node: Option[Node[A]], q: String): String = {
        if(node.isDefined)
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
}


class ArrayQueue[A: Manifest](emptyElement: A, capacity: Int = 4) extends Queue[A]
{
    var queue = new Array[A](capacity)
    var headIndex: Int = 0
    var tailIndex: Int = 0
    var qSize: Int = 0

    fillWithEmptyElement(queue)

    override def enqueue(item: A): Unit = {
        if(isFull) resize(queueLength * 2)
        queue(tailIndex) = item
        tailIndex = (tailIndex + 1) % queueLength
        qSize += 1
    }

    override def dequeue(): A = {
        if(isQueueEmpty)
            throw new Exception("Queue is empty")

        val item = queue(headIndex)
        queue(headIndex) = emptyElement
        headIndex = (headIndex + 1) % queueLength
        qSize -= 1
        if(shrinkable()) resize(queueLength / 2)
        item
    }

    override def isQueueEmpty: Boolean = qSize == 0

    private def shrinkable(): Boolean = qSize < queueLength / 4 && queueLength > capacity

    def queueLength(): Int = queue.length

    private def isFull: Boolean =  queueLength() == qSize

    private def fillWithEmptyElement(array: Array[A]): Unit = {
        for(i <- array.indices)
            array(i) = emptyElement
    }

    def queueSize(): Int = qSize

    private def resize(newLength: Int): Unit = {
        val newQueue = new Array[A](newLength)
        fillWithEmptyElement(newQueue)
        var index: Int = -1
        for(i <- 0 until qSize)
        {
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
            s += (if(item == emptyElement) "_ "  else queue(i) + " ")
        }
        s
    }

    override def iterator: Iterator[A] = new QueueIterator[A](queue)

    class QueueIterator[T](q: Array[T]) extends Iterator[T] {
        var index: Int = 0
        override def hasNext: Boolean = index < qSize
        override def next(): T = {
            val item = q((headIndex + index) % q.length)
            index += 1
            item
        }
    }
}

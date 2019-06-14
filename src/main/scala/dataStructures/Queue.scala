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


class ArrayQueue[A: Manifest](capacity: Int = 4) extends Queue[A]
{
    var queue = new Array[A](capacity)
    var headIndex: Int = 0
    var tailIndex: Int = 0

    override def enqueue(item: A): Unit = {
        if(isFull) resize(length * 2)
        queue(tailIndex) = item
        tailIndex = (tailIndex + 1) % length
    }

    override def dequeue(): A = {
        if(isQueueEmpty) {
            headIndex = 0
            tailIndex = 0
            throw new Exception("Queue is empty")
        }
        val item = queue(headIndex)
        headIndex = (headIndex + 1) % length
        if(shrinkable()) resize(length / 2)
        item
    }

    override def isQueueEmpty: Boolean = headIndex == tailIndex

    private def shrinkable(): Boolean = {
        if(headIndex < tailIndex)
            tailIndex - headIndex < length
        else if(headIndex > tailIndex)
            length - headIndex + tailIndex < length
        else
            true
    }

    private def length(): Int = queue.length

    private def isFull: Boolean = tailIndex % length == headIndex

    private def resize(length: Int): Unit = {
        val newQueue = new Array[A](length)
        var index: Int = -1
        for(i <- headIndex until tailIndex)
        {
            index += 1
            newQueue(index) = queue(i % length)
        }
        queue = newQueue
        headIndex = 0
        tailIndex = index
    }

    override def iterator: Iterator[A] = new QueueIterator[A](queue, headIndex, tailIndex)


    class QueueIterator[T](queue: Array[T], head: Int, tail: Int) extends Iterator[T] {
        override def hasNext: Boolean = false

        override def next(): T = {
            queue(head)
        }
    }
}

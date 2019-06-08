package dataStructures

trait Queue[A]
{
    def enqueue(item: A)
    def dequeue(): A
    def isEmpty(): Boolean
}

class LinkedListQueue[A] extends Queue[A] {
    
    private var head: Node[A] = _
    private var tail: Node[A] = _

    def enqueue(item: A) = {
        val newNode = Node[A](item, null)
        if(head == null) {
            head = newNode
            tail = head
        }
        else
        {
            head.next = newNode
        }
    }

    def dequeue(): A = {
        return tail.value
    }

    def isEmpty(): Boolean = head == null

    private case class Node[A](var value: A, var next: Node[A])

}
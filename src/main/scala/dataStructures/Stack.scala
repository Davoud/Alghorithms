package dataStructures

trait Stack[T]
{
    def push(item: T)
    def pop(): T
    def isEmpty: Boolean
}


case class Node[T](value: T, next: Node[T])

class LinkListStack[T] extends Stack[T]
{
    private var first: Node[T] = _
    private var size: Int = 0

    override def isEmpty: Boolean = first == null

    override def push(item: T): Unit = {
        if(isEmpty)
        {
            first = Node(item, null)
        }
        else
        {
            val newFirst = Node(item, first)
            first = newFirst
        }
    }

    override def pop(): T = {
        if(!isEmpty) {
            var oldFirst = first
            if(first.next != null)
            {
                first = first.next
            }
            val value = oldFirst.value
            oldFirst = null
            return value
        }
        throw new Exception("Stack is Empty")
    }

}
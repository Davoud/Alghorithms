package dataStructures
import scala.reflect.ClassTag
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
        var oldFirst = first;
        first = Node(item, oldFirst)      
    }

    override def pop(): T = {
        if(!isEmpty) {
            var item = first.value
            first = first.next
            return item
        }
        throw new Exception("Stack is Empty")
    }
}

class ArrayListStack[T: Manifest](initalCapacity: Int) extends Stack[T] 
{
    private var s = new Array[T](initalCapacity)
    private var N: Int = 0

    def isEmpty: Boolean = N == 0

    def pop(): T = {
        N -= 1
        var item = s(N)
        s(N) == null
        if(N > 0 && N == s.length/4) resize(s.length / 2)
        return item    
    }

    def push(item: T): Unit = {
        if(N == s.length) resize(2 * s.length)
        N += 1
        s(N) = item
    }
    
    private def resize(length: Int): Unit = {
        var copy = new Array[T](length)
        for(i <- 0 until s.length)
            copy(i) = s(i)
        s = copy
    }
}
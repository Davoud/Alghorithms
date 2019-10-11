package dataStructures.stacks

trait Stack[T] extends Iterable[T]
{
    def push(item: T)
    def pop(): T
    def isEmpty: Boolean
    
    def top: T
}




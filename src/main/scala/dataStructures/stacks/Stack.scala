package dataStructures.stacks

trait Stack[T]
{
    def push(item: T)
    def pop(): T
    def isEmpty: Boolean
}




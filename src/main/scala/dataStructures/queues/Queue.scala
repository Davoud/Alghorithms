package dataStructures.queues



trait Queue[A] extends Iterable[A]
{
    def enqueue(item: A)
    def dequeue(): A
    def isQueueEmpty: Boolean
}



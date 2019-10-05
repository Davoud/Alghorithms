package dataStructures.queues

class Deque[A] extends Iterable[A] {

  private var _first: Option[Node[A]] = None
  private var _last: Option[Node[A]] = None
  private var _size: Int = 0

  def isDequeEmpty: Boolean = _size == 0

  def dequeSize: Int = _size

  def addFirst(item: A): Unit = {
    _first = Some(new Node[A](item, _first, None))
    _size += 1
    if (_size == 1)
      _last = _first
  }

  def addLast(item: A): Unit = {
    val newLast = new Node[A](item, None, _last)
    _last.get.next = Some(newLast)
    _last = Some(newLast)
    _size += 1
    if (_size == 1)
      _first = _last
  }

  def removeFirst(): A = {
    if (_first.isDefined) {
      _size -= 1
      val firstNode = _first.get
      _first = firstNode.next
      return firstNode.value
    }
    throw new Exception("Deque is empty")
  }

  def removeLast(): A = {
    if (_last.isDefined) {
      _size -= 1
      val lastNode = _last.get
      _last = lastNode.prev

      if (_last.isDefined)
        _last.get.next = None
      else
        _first = None

      return lastNode.value
    }
    throw new Exception("Deque is empty")
  }

  def peakFirst(): Option[A] = if (_first.isDefined) Some(_first.get.value) else None

  def peakLast(): Option[A] = if (_last.isDefined) Some(_last.get.value) else None


  override def iterator: Iterator[A] = new DequeIterator[A](_first)

  class DequeIterator[T](private var node: Option[Node[T]]) extends Iterator[T] {
    override def hasNext: Boolean = node.isDefined

    override def next(): T = {
      val value = node.get.value
      node = node.get.next
      return value
    }
  }


  private case class Node[A](value: A, var next: Option[Node[A]], var prev: Option[Node[A]])
}

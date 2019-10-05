package dataStructures.queues

import edu.princeton.cs.algs4.StdRandom


class RandomizedQueue[A: Manifest](capacity: Int = 4) extends Iterable[A] {

  private var queue = new Array[Option[A]](capacity)
  private var tailIndex: Int = 0

  def isQueueEmpty: Boolean = tailIndex == 0

  def queueSize: Int = tailIndex

  def enqueue(item: A): Unit = {
    if (tailIndex == queue.length) resize(queue.length * 2)
    if (tailIndex == queue.length / 4) resize(queue.length / 2)

    if (tailIndex == 0) {
      queue(tailIndex) = Some(item)
    }
    else {
      queue(tailIndex) = Some(item)
      exchange(queue, tailIndex, StdRandom.uniform(tailIndex))
    }
    tailIndex += 1
  }

  private def resize(length: Int): Unit = {
    val newQueue = new Array[Option[A]](length)
    for (i <- 0 until tailIndex)
      newQueue(i) = queue(i)
    queue = newQueue
  }

  private def exchange(a: Array[Option[A]], i: Int, j: Int): Unit = {
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }

  def dequeue(): A = {
    if (tailIndex > 0) {
      tailIndex -= 1
      val item = queue(tailIndex).get
      queue(tailIndex) = None
      return item
    }
    throw new NoSuchElementException("Queue is empty")
  }

  def sample(): A =
    if (tailIndex > 0)
      queue(StdRandom.uniform(tailIndex)).get
    else
      throw new NoSuchElementException("Queue is empty")

  override def iterator: Iterator[A] = new QueueIterator[A](queue, tailIndex)

  class QueueIterator[T: Manifest](q: Array[Option[T]], tailIndex: Int) extends Iterator[T] {

    var index: Int = 0
    val shuffledQueue = shuffle(q)

    private def shuffle(a: Array[Option[T]]): Array[T] = {
      val q = new Array[T](tailIndex)

      for (i <- 0 until tailIndex)
        q(i) = a(i).get

      q.indices.foreach(i => exchange(q, i, StdRandom.uniform(i + 1)))
      return q
    }

    private def exchange(a: Array[T], i: Int, j: Int): Unit = {
      val temp = a(i)
      a(i) = a(j)
      a(j) = temp
    }

    override def hasNext: Boolean = index < tailIndex

    override def next(): T = {
      val item = shuffledQueue(index)
      index += 1
      item
    }
  }

}

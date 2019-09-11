package dataStructures

import org.scalatest.{FlatSpec, Matchers}

class QueueTest extends FlatSpec with Matchers {

  def NewQueue[A: Manifest](e: A): Queue[A] = new ArrayQueue[A]()


  "Queue" should "emit items in an FIFO order" in {
      val q = NewQueue[Int](-1)
      q.enqueue(1)
      q.enqueue(2)
      q.enqueue(3)
      q.enqueue(4)
      q.enqueue(5)
      q.enqueue(6)
      q.enqueue(7)
      q.dequeue() should be (1)
      q.dequeue() should be (2)
      q.dequeue() should be (3)
      q.dequeue() should be (4)
      q.dequeue() should be (5)
      q.dequeue() should be (6)
      q.dequeue() should be (7)
  }

  it should "reflect its emptiness properly " in {
      val q = NewQueue[Int](-1)
      q.isQueueEmpty should be (true)
      q.enqueue(0)
      q.isQueueEmpty should be (false)
      q.dequeue()
      q.isQueueEmpty should be (true)
  }

  it should "throw Exception when it is empty upon dequeue" in {
    val q = NewQueue[String](null)
    a [Exception] should be thrownBy { q.dequeue() }
  }

  it should "be able to iterate over" in {
    val q = NewQueue[String](null)
    val words = Vector("My ", "name ", "is ", "Queue!")

    for(word <- words)
      q enqueue word

    var i = 0
    for(item <- q) {
      item should be (words(i))
      i += 1
    }

  }


}

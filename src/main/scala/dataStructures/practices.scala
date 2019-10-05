package dataStructures

import dataStructures.queues.Queue
import dataStructures.stacks.{ArrayListStack, LinkedListStack, Stack}

class QueueWithStacks[A: Manifest] extends Queue[A] {

  private val input: ArrayListStack[A] = new ArrayListStack[A](4)
  private val output: ArrayListStack[A] = new ArrayListStack[A](4)
  override def enqueue(item: A): Unit = {
    input.push(item)
  }

  override def dequeue(): A = {
    if(output.isEmpty) {
      while(!input.isEmpty)
        output.push(input.pop())
    }
    output.pop()

  }

  override def isQueueEmpty: Boolean = input.isEmpty && output.isEmpty

  override def iterator: Iterator[A] = ???
}

class StackWithMax extends Stack[Double] {
  
  private def stack: LinkedListStack[Double] = new LinkedListStack[Double]()

  private var max: Double = Double.MinValue

  override def push(item: Double): Unit = {
    if(item > max)
      max = item
    stack.push(item)
  }

  override def pop(): Double = {
    val v = stack.pop()
    if(v == max)
       findMax()
    v
  }

  private def findMax(): Unit = ???


  override def isEmpty: Boolean = stack.isEmpty

  def Maximum: Double = max
}


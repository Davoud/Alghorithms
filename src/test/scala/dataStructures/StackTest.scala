package dataStructures

import dataStructures.stacks.{ArrayListStack, LinkedListStack, Stack}
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer

class StackTest extends FunSuite{
  
  def GetEmptyStack[T](): Stack[T] = new LinkedListStack[T]

  test("Push, Push, Push, Pop, Pop, Pop") {
    val s = GetEmptyStack[Int]
    s.push(1)
    s.push(2)
    s.push(3)
    assert(s.pop() === 3)
    assert(s.pop() === 2)
    assert(s.pop() === 1)
  }

  test("Empty stack") {
    val s = GetEmptyStack[Int]()
    assert(s.isEmpty === true)
    s.push(1)
    assert(s.isEmpty === false)
    s.pop()
    assert(s.isEmpty === true)
  }

  test("Poping an empty stack") {
    val s = GetEmptyStack[Int]()
    val thrown = intercept[Exception] {s.pop()}
    assert(thrown.getMessage === "Stack is Empty")
  }
  
  test("Top of Stack") {
    val s = GetEmptyStack[String]
    
    s.push("A")
    s.push("B")
    
    assert(s.top == "B")
    
    s.push("C")
    assert(s.top == "C")
    
    s.pop()
    assert(s.top == "B")
    
  }
  
  test("Iterating Stack") {
    val s = GetEmptyStack[String]
    s.push("A")
    s.push("B")
    s.push("C")
    
    var list = new ListBuffer[String]
    for (item <- s)
      list += item
    
    assert(list(0) == "A")
    assert(list(1) == "B")
    assert(list(2) == "C")
    
  }
  
}

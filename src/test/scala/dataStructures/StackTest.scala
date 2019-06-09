package dataStructures

import org.scalatest.FunSuite

class StackTest extends FunSuite{

  def GetEmptyStack[T](): Stack[T] = new LinkListStack[T]()

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
}

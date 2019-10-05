package dataStructures.stacks

class ArrayListStack[T: Manifest](initalCapacity: Int) extends Stack[T] {
	private var s = new Array[T](initalCapacity)
	private var N: Int = 0
	
	def isEmpty: Boolean = N == 0
	
	def pop(): T = {
		N -= 1
		var item = s(N)
		s(N) == null
		if (N > 0 && N == s.length / 4) resize(s.length / 2)
		return item
	}
	
	def push(item: T): Unit = {
		if (N == s.length) resize(2 * s.length)
		N += 1
		s(N) = item
	}
	
	private def resize(length: Int): Unit = {
		var copy = new Array[T](length)
		for (i <- 0 until s.length)
			copy(i) = s(i)
		s = copy
	}
}
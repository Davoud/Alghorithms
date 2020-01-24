package dataStructures.stacks

class ArrayListStack[T: Manifest](initialCapacity: Int) extends Stack[T] {
	private var s = new Array[T](initialCapacity)
	private var N: Int = 0
	
	override def isEmpty: Boolean = N == 0
	
	def pop(): T = {
		N -= 1
		val item = s(N)
		s(N) == null
		if (N > 0 && N == s.length / 4) resize(s.length / 2)
		item
	}
	
	def push(item: T): Unit = {
		if (N == s.length) resize(2 * s.length)
		N += 1
		s(N) = item
	}
	
	override def top(): T = s(N - 1)
	
	private def resize(length: Int): Unit = {
		val copy = new Array[T](length)
		for (i <- s.indices)
			copy(i) = s(i)
		s = copy
	}
	
	override def iterator: Iterator[T] = s.iterator
	
	
}
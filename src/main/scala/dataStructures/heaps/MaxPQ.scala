package dataStructures.heaps

class MaxPQ[Key: Manifest](keys: Option[Array[Key]] = None)(implicit ordering: Ordering[Key]) {
	
	var heap: Array[Option[Key]] = new Array[Option[Key]](4)
	fill(heap, None)
	private var N: Int = 0
	
	if (keys.isDefined) {
		heap = new Array[Option[Key]](keys.get.length + 1)
		
		for (i <- keys.get.indices)
			heap(i) = Some(keys.get(i))
		
		for (k <- N / 2 to 1 by -1)
			sink(k)
	}
	
	
	def insert(v: Key): Unit = {
		N += 1
		if (N == heap.length) resize(N * 2)
		heap(N) = Some(v)
		swim(N)
	}
	
	
	private def resize(newLength: Int): Unit = {
		val newHeap = new Array[Option[Key]](newLength)
		fill(newHeap, None)
		
		for (i <- 0 until Math.min(newLength, heap.length))
			newHeap(i) = heap(i)
		heap = newHeap
	}
	
	def delMax(): Key = {
		val max = heap(1).get
		swap(1, N)
		N -= 1
		sink(1)
		heap(N + 1) = None
		if (N <= heap.length / 4 && N < 16) resize(heap.length / 2)
		max
	}
	
	def isEmpty: Boolean = size == 0
	
	def max: Key = heap(1).get
	
	def size: Int = N
	
	private def swim(n: Int): Unit = {
		var k = n
		while (k > 1 && less(k / 2, k)) {
			swap(k, k / 2)
			k = k / 2
		}
	}
	
	@inline def less(i: Int, j: Int): Boolean = ordering.lt(heap(i).get, heap(j).get)
	
	@inline def swap(i: Int, j: Int): Unit = {
		val t = heap(i)
		heap(i) = heap(j)
		heap(j) = t
	}
	
	private def sink(n: Int): Unit = {
		var k = n
		while (2 * k <= N) {
			var j = 2 * k
			if (j < N && less(j, j + 1)) j += 1
			if (!less(k, j)) return
			swap(k, j)
			k = j
		}
	}
	
	override def toString: String = {
		var s = ""
		for (i <- heap.indices)
			if (heap(i).isDefined)
				s += s"${heap(i).get} "
		s
	}
	
	private def fill(a: Array[Option[Key]], v: Option[Key]): Unit = {
		for (i <- a.indices)
			a(i) = v
	}
}


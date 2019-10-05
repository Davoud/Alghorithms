package dataStructures.heaps

object HeapTest {
	def Go(size: Int): Unit = {
		val q = new MaxPQ[Int]()
		
		val array = new Array[Int](size)
		
		for (i <- array.indices)
			array(i) = i
		
		sorting.Sorting.shuffle(array)
		
		for (i <- array.indices)
			q.insert(array(i))
		
		println(q)
		
		while (!q.isEmpty) {
			println(q.delMax())
		}
		
	}
	
	
}

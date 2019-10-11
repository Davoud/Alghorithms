package dataStructures.trees

object BSTTest {
	
	
	def Test(): Unit = {
		
		for (i <- 26 to 26) {
			val tree = new Bst[Char, Char]()
			
			for (v <- sampleChars(i))
				tree.put(v, v)
			
			val vis = new TreeVisualizer(tree)
			vis.print
			println()
		}
	}
	
	
	private def sampleChars(length: Int = 7): Array[Char] = {
		val array = new Array[Char](length)
		val cc = 'A'
		for (i <- 0 until length) {
			array(i) = (cc + i).toChar
		}
		sorting.Sorting.shuffle(array)
		array
	}
	
	def print[Key, Value](bst: Bst[Key, Value]): Unit = {
		val values = bst.values(TraversMode.InOrder)
		println(values.foldRight("")((v, s) => s"$v $s"))
	}
}

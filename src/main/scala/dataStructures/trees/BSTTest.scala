package dataStructures.trees

object BSTTest {
	def Test1() = {
		
		val tree = new Bst[Int, Char]()
		for (v <- sampleValues)
			tree.put(v, v.toChar)
		
		println(tree.values(TraversMode.InOrder).foldRight("")((c: Char, s: String) => s"$c $s"))
		println(tree.values(TraversMode.PreOrder).foldRight("")((c: Char, s: String) => s"$c $s"))
		println(tree.values(TraversMode.PostOrder).foldRight("")((c: Char, s: String) => s"$c $s"))
		
	}
	
	def Test2(): Unit = {
		val t = new Bst[Int, Char]()
		t.put(5, 'E')
		println(t.size)
		print(t)
		t.put(1, 'A')
		println(t.size)
		print(t)
		t.put(10, 'I')
		println(t.size)
		print(t)
	}
	
	def Test3(): Unit = {
		val tree = new Bst[Int, Char]()
		val visualizer = new TreeVisualizer(tree)
		for (i <- 1 to 10) {
			println(i)
			visualizer.print()
			println()
		}
	}
	
	def Test(): Unit = {
		
		for (i <- 9 to 11) {
			val tree = new Bst[Char, Char]()
			
			for (v <- sampleChars(i))
				tree.put(v, v)
			
			val vis = new TreeVisualizer(tree)
			vis.print()
			println()
		}
	}
	
	private def sampleValues: Array[Int] = {
		val array = new Array[Int](10)
		for (i <- 0 until 7)
			array(i) = i + 65
		sorting.Sorting.shuffle(array)
		array
	}
	
	private def sampleChars(length: Int = 7): Array[Char] = {
		val array = new Array[Char](length)
		var cc = 'A'
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

package dataStructures.trees

object BSTTest {
	
	def Test(): Unit = {
		
		var tree = new RedBlackTree[Char, Char]()
		
		var input: Char = ' '
		do {
			input = scala.io.StdIn.readChar()
			if (input.isDigit) {
				tree = new RedBlackTree[Char, Char]()
				println("Cleared Nodes.")
			}
			else if (input.isLetter) {
				tree.put(input, input)
				new TreeVisualizer(tree).print
			}
		} while (input != '.')
		
	}
	
	def TestRedBlack(): Unit = {
		val tree = new RedBlackTree[Char, Char]()
		sampleChars(5, 1).foreach(v => tree.put(v, v))
		new TreeVisualizer(tree).print
		println(tree.size)
	}
	
	def TestDelete(): Unit = {
		
		for (i <- 26 to 26) {
			val tree = new Bst[Char, Char]()
			sampleChars(i, 2).foreach(v => tree.put(v, v))
			new TreeVisualizer(tree).print
			println(tree.size)
			
			tree.delete('M')
			new TreeVisualizer(tree).print
			println(tree.size)

			tree.delete('K')
			new TreeVisualizer(tree).print
			println(tree.size)
		}
	}
	
	private def sampleChars(length: Int = 26, gap: Int = 1): Array[Char] = {
		val list = new scala.collection.mutable.ListBuffer[Char]
		val cc = 'A'
		(0 until length by gap).foreach(i => list += (cc + i).toChar)
		val array = list.toArray
		sorting.Sorting.shuffle(array)
		array
	}
	
	def print[Key, Value](bst: Bst[Key, Value]): Unit = {
		val values = bst.values(bst.TraversMode.InOrder)
		println(values.foldRight("")((v, s) => s"$v $s"))
	}
}

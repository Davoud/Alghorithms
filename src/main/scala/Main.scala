import sliderPuzzle.Board

object Main extends App {
	
	
	TestBoard()
	
	def Goal3x3: Board = new Board(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 0)))
	
	def Sample3x3: Board = new Board(Array(Array(8, 0, 3), Array(4, 1, 2), Array(7, 6, 5)))
	
	def TestBoard() = {
		
		val b = Sample3x3
		
		println(b)
		
		println("Neighbors:")
		for (n <- b.neighbors())
			println(n)
		
		println(" - - - ")
		println(b)
		
	}

	
}


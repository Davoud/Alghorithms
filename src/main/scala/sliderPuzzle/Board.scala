package sliderPuzzle

class Board(inputTiles: Array[Array[Int]]) {
	
	val n = inputTiles.length
	val tiles = Array.ofDim[Int](n, n)
	
	for {
		i <- 0 until n
		j <- 0 until n
	} tiles(i)(j) = inputTiles(i)(j)
	
	
	override def toString: String = {
		var str = s"$n\n"
		
		for (i <- tiles.indices) {
			for (j <- tiles(i).indices)
				str += pad(tiles(i)(j))
			str += "\n"
		}
		
		str
	}
	
	def dimension: Int = n
	
	def hamming: Int = {
		var h = 0
		
		for (i <- tiles.indices)
			for (j <- tiles(i).indices)
				if (tiles(i)(j) != 0 && tiles(i)(j) != goal(i)(j)) h += 1
		h
	}
	
	@inline private def distance(i: Int, j: Int): Int = {
		val value = tiles(i)(j)
		if (value == 0 || value == goal(i)(j)) return 0
		val h = home(value)
		return math.abs(i - h._1) + math.abs(j - h._2)
	}
	
	def manhattan: Int = {
		var m = 0
		for (i <- 0 until n)
			for (j <- 0 until n)
				m += distance(i, j)
		m
	}
	
	@inline private def goal(i: Int)(j: Int): Int = if (i == (n - 1) && i == j) 0 else (i * n) + j + 1
	
	@inline private def home(v: Int): (Int, Int) = ((v - 1) / n, (v - 1) % n)
	
	
	def isGoal: Boolean = this == getGoal
	
	def getGoal: Board = {
		val g = Array.ofDim[Int](n, n)
		for {
			i <- 0 until n
			j <- 0 until n
		} g(i)(j) = goal(i)(j)
		new Board(g)
	}
	
	
	override def equals(obj: Any): Boolean = {
		
		if (obj == null) return false
		if (!obj.isInstanceOf[Board]) return false
		
		val other = obj.asInstanceOf[Board]
		if (other eq this) return true
		if (dimension != other.dimension) return false
		for {
			i <- 0 until n
			j <- 0 until n
		} if (tiles(i)(j) != other.tiles(i)(j)) return false
		
		true
	}
	
	def newBoard(currentBlank: (Int, Int), newBlank: (Int, Int)): Board = {
		val board = new Board(tiles)
		board.tiles(currentBlank._1)(currentBlank._2) = tiles(newBlank._1)(newBlank._2)
		board.tiles(newBlank._1)(newBlank._2) = 0
		return board
	}
	
	def neighbors(): Iterable[Board] = {
		
		val (i, j) = blank
		
		val neighborXY = Array((i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1))
			.filter(p => p._1 >= 0 && p._1 < n && p._2 >= 0 && p._2 < n)
		
		val neighboringBoards = new Array[Board](neighborXY.length)
		
		for (b <- neighboringBoards.indices) {
			neighboringBoards(b) = newBoard((i, j), neighborXY(b))
		}
		
		neighboringBoards
		
	}
	
	private def blank: (Int, Int) = {
		for {
			i <- 0 until n
			j <- 0 until n
		} if (tiles(i)(j) == 0) return (i, j)
		
		throw new Exception("There is no blank on the board!")
	}
	
	
	private def pad(n: Int): String = {
		val len: Int = n.toString().length
		var str = ""
		for (_ <- 0 until (5 - len))
			str += " "
		str + n
	}
	
	
}

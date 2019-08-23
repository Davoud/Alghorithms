package sliderPuzzle

import scala.collection.mutable.ListBuffer

case class Tile(value: Int, rowIndex: Int, columnIndex: Int)

class Board(inputTiles: Array[Array[Int]], modifier: Option[Tile] = None) {
	
	private val n = inputTiles.length
	private val tiles = Array.ofDim[Int](n, n)
	private var _manhattan = 0
	private var _hamming = 0
	private var _goal = true
	
	for (row <- 0 until n)
		for (col <- 0 until n) {
			val tile = setTile(Tile(inputTiles(row)(col), row, col), modifier)
			_hamming += oneIfDisplaced(tile)
			_manhattan += distanceFromHome(tile)
			if(tile.value != goal(row, col))
				_goal = false
		}
	
	private def setTile(tile: Tile, modifier: Option[Tile]): Tile = {
		var value = 0
		if (modifier.isDefined && tile.value == 0)
			value = modifier.get.value
		
		else if (modifier.isDefined && tile.rowIndex == modifier.get.rowIndex && tile.columnIndex == modifier.get.columnIndex)
			value = 0
		
		else
			value = tile.value
		
		tiles(tile.rowIndex)(tile.columnIndex) = value
		Tile(value, tile.rowIndex, tile.columnIndex)
	}
	
	override def toString: String = {
		var str = s"$n (Manhattan: ${_manhattan}, Hamming: ${_hamming}) \n"
		
		for (i <- tiles.indices) {
			for (j <- tiles(i).indices) {
				val v = tiles(i)(j)
				str += (if (v == 0) pad(" ") else pad(v.toString))
			}
			str += "\n"
		}
		
		str
	}
	
	def dimension: Int = n
	
	def manhattan: Int = _manhattan
	
	def hamming: Int = _hamming
	
	
	@inline private def distanceFromHome(tile: Tile): Int = {
		if (tile.value == 0 || tile.value == goal(tile.rowIndex, tile.columnIndex)) return 0
		val homeTile = home(tile.value)
		math.abs(tile.rowIndex - homeTile.rowIndex) + math.abs(tile.columnIndex - homeTile.columnIndex)
	}
	
	@inline private def oneIfDisplaced(tile: Tile): Int =
		if (tile.value != 0 && tile.value != goal(tile.rowIndex, tile.columnIndex)) 1 else 0
	
	@inline private def goal(i: Int, j: Int): Int = if (i == (n - 1) && i == j) 0 else (i * n) + j + 1
	
	@inline private def home(v: Int): Tile = Tile(v, (v - 1) / n, (v - 1) % n)
	
	def isGoal: Boolean = _goal
	
	def getGoal: Board = {
		val g = Array.ofDim[Int](n, n)
		for {
			i <- 0 until n
			j <- 0 until n
		} g(i)(j) = goal(i, j)
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
	
	
	def neighbors: Iterable[Board] = {
		
		val boards = new ListBuffer[Board]
		val (i, j) = blank
		
		val neighborXY = Array((i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1))
		
		for (xy <- neighborXY.filter(p => p._1 >= 0 && p._1 < n && p._2 >= 0 && p._2 < n))
			boards += new Board(tiles, Some(Tile(tiles(xy._1)(xy._2), xy._1, xy._2)))
		
		boards
	}
	
	private def blank: (Int, Int) = {
		for {
			i <- 0 until n
			j <- 0 until n
		} if (tiles(i)(j) == 0) return (i, j)
		
		throw new Exception("There is no blank on the board!")
	}
	
	
	private def pad(n: String): String = {
		val len: Int = n.length
		var str = ""
		for (_ <- 0 until (5 - len))
			str += " "
		str + n
	}
	
	def twin(): Board = {
		val board = Array.ofDim[Int](n, n)
		var firstNonBlank: Option[Tile] = None
		var secondNonBlank: Option[Tile] = None
		
		for {
			i <- 0 until n
			j <- 0 until n
		} {
			val value = tiles(i)(j)
			board(i)(j) = value
			
			if (value != 0)
				if (firstNonBlank.isEmpty)
					firstNonBlank = Some(Tile(value, i, j))
				else if (secondNonBlank.isEmpty)
					secondNonBlank = Some(Tile(value, i, j))
		}
		
		board(firstNonBlank.get.rowIndex)(firstNonBlank.get.columnIndex) = secondNonBlank.get.value
		board(secondNonBlank.get.rowIndex)(secondNonBlank.get.columnIndex) = firstNonBlank.get.value
		
		new Board(board)
		
	}
	
	
}

package sliderPuzzle

import edu.princeton.cs.algs4.MinPQ
import scala.collection.mutable.ListBuffer


class Solver(initial: Board) {
	
	val queue: MinPQ[SearchNode] = new MinPQ[SearchNode](new ManhattanCompare)
	var boards: ListBuffer[Board] = new ListBuffer[Board]()
	
	def isSolvable: Boolean = {
		
		queue.insert(SearchNode(initial, 0, None))
		queue.insert(SearchNode(initial.twin(), 0, None, true))
		
		while (!queue.isEmpty) {
			val min = queue.delMin()
			if (min.board.isGoal) {
				if (min.isTwin) return false
				trackSolution(min)
				return true
			}
			updateAStar(min)
		}
		false
	}
	
	def moves: Int = boards.length - 1
	
	def solution: Iterable[Board] = boards.reverse
	
	def trackSolution(node: SearchNode): Unit = {
		boards += node.board
		if (node.prev.isDefined)
			trackSolution(node.prev.get)
	}
	
	private def updateAStar(min: SearchNode): Unit =
		for (neighbor <- min.board.neighbors)
			if (min.prev.isEmpty || min.prev.get.board != neighbor)
				queue.insert(SearchNode(neighbor, min.moves + 1, Some(min), min.isTwin))
	
	case class SearchNode(board: Board, moves: Int, prev: Option[SearchNode], isTwin: Boolean = false) {
		def priority: Int = board.manhattan + moves
	}
	
	class ManhattanCompare extends Ordering[SearchNode] {
		override def compare(x: SearchNode, y: SearchNode): Int =
			if (x.priority < y.priority) -1 else if (x.priority > y.priority) 1 else 0
	}
}


object SolverTest {
	
	def Goal3x3: Board = new Board(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 0)))
	def Other3x3 = new Board(Array(Array(0, 1, 3), Array(4, 2, 5), Array(7, 8, 6)))
	
	def Other3x3NotSolvable = new Board(Array(Array(0, 3, 1), Array(4, 2, 5), Array(7, 8, 6)))
	def Sample3x3: Board = new Board(Array(Array(8, 0, 3), Array(4, 1, 2), Array(7, 6, 5)))
	
	def Sample3x3NotSolvable: Board = new Board(Array(Array(8, 0, 3), Array(4, 2, 1), Array(7, 6, 5)))
	
	def Sample4x4: Board = new Board(Array(
		Array(1, 0, 2, 8),
		Array(9, 6, 3, 4),
		Array(5, 7, 11, 12),
		Array(13, 10, 15, 14),
	))
	
	def run(sample: Board): Unit = {
		val s = new Solver(sample)
		
		if(s.isSolvable) {
			println("Moves:" + s.moves)
			for (board <- s.solution)
				println(board)
		}
		else
			println("Not Solvable")
	}
}
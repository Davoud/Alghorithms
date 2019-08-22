package sliderPuzzle

import edu.princeton.cs.algs4.MinPQ
import scala.collection.mutable.ListBuffer

class Solver(initial: Board) {
	
	val queue: MinPQ[SearchNode] = new MinPQ[SearchNode](new ManhattanCompare)
	val twinQueue: MinPQ[SearchNode] = new MinPQ[SearchNode](new ManhattanCompare)
	var boards: ListBuffer[Board] = new ListBuffer[Board]()
	
	def isSolvable: Boolean = {
		
		if(initial.isGoal)
			return true
		
		queue.insert(SearchNode(initial, 0, None))
		twinQueue.insert(SearchNode(initial.twin(), 0, None))
		
		while(!queue.isEmpty && !twinQueue.isEmpty) {
			val min = queue.delMin()
			val minTwin = twinQueue.delMin()
			
			if(minTwin.board.isGoal)
				return false
			
			boards += min.board
			
			if(min.board.isGoal)
				return true
			
			update(queue, min)
			update(twinQueue, minTwin)
			
		}
		
		false
	}
	
	private def update(queue: MinPQ[SearchNode], min: SearchNode): Unit =
	{
		for(neighbor <- min.board.neighbors())
		{
			if(min.prev.isEmpty)
				queue.insert(SearchNode(neighbor, min.moves + 1, Some(min.board)))
			else if(min.prev.get != neighbor)
				queue.insert(SearchNode(neighbor, min.moves + 1, Some(min.board)))
		}
	}
	def moves: Int = boards.length - 1
	
	def solution: Iterable[Board] = boards
	
	case class SearchNode(board: Board, moves: Int, prev: Option[Board])
	
	class ManhattanCompare extends Ordering[SearchNode] {
		override def compare(x: SearchNode, y: SearchNode): Int = {
			val priorityOfx = x.board.manhattan + x.moves
			val priorityOfy = y.board.manhattan + y.moves
			
			if(priorityOfx < priorityOfy) -1
			else if(priorityOfx > priorityOfy) 1
			else 0
		}
	}
	
}

object SolverTest {
	
	def Goal3x3: Board = new Board(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 0)))
	def Other3x3 = new Board(Array(Array(0, 1, 3), Array(4, 2, 5), Array(7, 8, 6)))
	def Sample3x3: Board = new Board(Array(Array(8, 0, 3), Array(4, 1, 2), Array(7, 6, 5)))
	
	
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
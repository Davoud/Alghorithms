package sliderPuzzle

import org.scalatest.{FlatSpec, Matchers}


class BoardTest extends FlatSpec with Matchers {
	
	def Goal3x3: Board = new Board(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 0)))
	
	def Sample3x3: Board = new Board(Array(Array(8, 1, 3), Array(4, 0, 2), Array(7, 6, 5)))
	
	"Goal Board" should "confirm with isGoal" in {
		Goal3x3.isGoal should be(true)
	}
	
	it should "report hamming as 0" in {
		Goal3x3.hamming should be(0)
	}
	
	it should "report manhattan as 0" in {
		Goal3x3.manhattan should be(0)
	}
	
	"Two identical boards" should "be equal" in {
		val b1 = new Board(Array(Array(1, 0), Array(2, 3)))
		val b2 = new Board(Array(Array(1, 0), Array(2, 3)))
		b1 == b2 should be(true)
		b2 == b1 should be(true)
	}
	
	"Two different boards" should "be unequal" in {
		val b1 = new Board(Array(Array(1, 2), Array(0, 3)))
		val b2 = new Board(Array(Array(1, 0), Array(2, 3)))
		b1 == b2 should be(false)
		b2 == b1 should be(false)
	}
	
	"Sample board" should "report hamming as 5" in {
		Sample3x3.hamming should be(5)
	}
	
	it should "report manhattan as 10" in {
		Sample3x3.manhattan should be(10)
	}
	
	it should "returns 4 neighbors" in {
		Sample3x3.neighbors.size should be(4)
	}
	
	
}

package sorting

import edu.princeton.cs.algs4.StdDraw

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class Point(val x: Int, val y: Int) extends Ordered[Point] {
	
	def draw(): Unit = {
		StdDraw.point(x, y)
	}
	
	def drawTo(that: Point): Unit = StdDraw.line(x, y, that.x, that.y)
	
	def slopeTo(that: Point): Double = {
		if (x == that.x && y == that.y) return Double.NegativeInfinity
		if (x == that.x) return Double.PositiveInfinity
		(that.y - y).toDouble / (that.x - x)
		
	}
	
	override def compare(that: Point): Int = {
		if (y < that.y) -1
		else if (y > that.y) 1
		else if (x < that.x) -1
		else if (x > that.x) 1
		else 0
	}
	
	override def toString: String = s"($x, $y)"
	
	def slopeOrder(): Ordering[Point] = new SlopeOrdering(this)
	
	class SlopeOrdering(p: Point) extends Ordering[Point] {
		override def compare(p1: Point, p2: Point): Int = {
			val slopToP1 = p slopeTo p1
			val slopToP2 = p slopeTo p2
			if (slopToP1 < slopToP2) -1 else if (slopToP1 > slopToP2) 1 else 0
		}
	}
	
}

class LineSegment(val start: Point, val end: Point) {
	def draw(): Unit = start drawTo end
	
	def slop: Double = start slopeTo end
	
	override def toString: String = s"$start -> $end"
	
	override def hashCode(): Int = throw new UnsupportedOperationException
}

class BruteCollinearPoints(val points: Seq[Point]) {
	
	private val lines = new ListBuffer[LineSegment]
	
	private val len = points.length
	if (len < 4)
		throw new IllegalArgumentException("At least 4 points are required!")
	
	private val addedSlopes = mutable.Set[Double]()
	
	for (p <- 0 until len)
		for (q <- p + 1 until len)
			for (r <- q + 1 until len)
				for (s <- r + 1 until len) {
					
					val pq = slopeOf(p, q)
					val qr = slopeOf(q, r)
					val rs = slopeOf(r, s)
					
					if (pq == qr && qr == rs && !(addedSlopes contains pq)) {
						addedSlopes += pq
						lines += new LineSegment(points(p), points(s))
					}
				}
	
	private def slopeOf(p: Int, q: Int): Double = points(p) slopeTo points(q)
	
	def numberOfSegments(): Int = lines.length
	
	def segments(): Seq[LineSegment] = lines
}

class FastCollinearPoints(points: Array[Point]) {
	
	val lines = new ListBuffer[LineSegment]()
	
	for (p <- points.indices) {
		
		val point = points(p)
		val others = points.filter(i => i != point)
		
		Sorting.insertion(others, point.slopeOrder())
		val slopes = SlopesWith(point, others)
		AddLineSegments(slopes, others)
		
	}
	
	
	def numberOfSegments(): Int = lines.size
	
	def segments(): Iterable[LineSegment] = lines
	
	private def SlopesWith(point: Point, others: Array[Point]): Array[Double] = {
		val slopes = new Array[Double](others.length)
		for (i <- others.indices)
			slopes(i) = point slopeTo others(i)
		slopes
	}
	
	private def AddLineSegments(slopes: Array[Double], others: Array[Point]): Unit = {
		
		var lastSlope = slopes(0)
		var count = 1
		var indexOfLast = 0
		
		for (s <- 1 until slopes.length) {
			val slope = slopes(s)
			
			if (slope == lastSlope) {
				count += 1
				if (count == 3) {
					val start = others(indexOfLast)
					val end = others(s)
					if (isNewLine(start, end))
						lines += new LineSegment(start, end)
				}
			}
			else {
				lastSlope = slope
				indexOfLast = s
				count = 1
			}
		}
	}
	
	private def isNewLine(s: Point, e: Point): Boolean = {
		for (segment <- lines)
			if (segment.start == s && segment.end == e) return false
		
		true
	}
	
	
}

object CollinearPoints {
	
	def TestFast(): Unit = {
		
		val points = sample2()
		
		StdDraw.enableDoubleBuffering()
		StdDraw.setXscale(0, 32768)
		StdDraw.setYscale(0, 32768)
		StdDraw.setPenRadius(0.01)
		
		for (p <- points) p.draw
		
		StdDraw.show()
		
		StdDraw.setPenColor(StdDraw.PINK)
		StdDraw.setPenRadius(0.003)
		
		val collinear = new FastCollinearPoints(points)
		println(s"Segments: ${collinear.numberOfSegments()}")
		for (segment <- collinear.segments) {
			println(segment)
			segment.draw()
		}
		
		StdDraw.show()
		
	}
	
	def TestBruteForce(): Unit = {
		
		val points = sample2()
		
		Sorting.merge(points)
		
		StdDraw.enableDoubleBuffering()
		StdDraw.setXscale(0, 32768)
		StdDraw.setYscale(0, 32768)
		StdDraw.setPenRadius(0.01)
		
		for (p <- points) p.draw
		
		StdDraw.show()
		
		StdDraw.setPenColor(StdDraw.PINK)
		StdDraw.setPenRadius(0.003)
		
		val collinear = new BruteCollinearPoints(points)
		println(s"Segments: ${collinear.numberOfSegments()}")
		for (segment <- collinear.segments) {
			println(segment)
			segment.draw()
		}
		
		StdDraw.show()
		
	}
	
	def sample1(): Array[Point] = Array(
		new Point(19000, 10000),
		new Point(18000, 10000),
		new Point(32000, 10000),
		new Point(21000, 10000),
		new Point(1234, 5678),
		new Point(14000, 10000)
	)
	
	def sample2(): Array[Point] = Array(
		new Point(10000, 0),
		new Point(0, 10000),
		new Point(3000, 7000),
		new Point(7000, 3000),
		new Point(20000, 21000),
		new Point(3000, 4000),
		new Point(14000, 15000),
		new Point(6000, 7000)
	)
}
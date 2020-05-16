package graphAssignments

import edu.princeton.cs.algs4.{Picture, Stopwatch}

import scala.annotation.tailrec
import scala.collection.mutable


class SeamCarver(private val pic: Picture) {
	require(pic != null, s"input picture can not be null")
	
	private var _pic: Option[Picture] = None
	
	def picture: Picture = _pic.getOrElse(pic)
	
	private var energies: EnergyMatrix = new EnergyMatrix(picture)
	
	var width: Int = pic.width()
	var height: Int = pic.height()
	
	private def set(p: Picture): Unit = {
		_pic = Some(p)
		width = p.width()
		height = p.height()
		last = Xy(width, height)
		energies = new EnergyMatrix(p)
	}
	
	private val first: Xy = Xy(-1, -1)
	private var last: Xy = Xy(width, height)
	
	def findHorizontalSeam: List[Int] = findSeam(energies.horizontalTopOrder, vAdj, p => p.y)
	
	def findVerticalSeam: List[Int] = findSeam(energies.verticalTopOrder, hAdj, p => p.x)
	
	private def findSeam(topological: Seq[Xy], adj: Xy => Seq[Xy], selector: Xy => Int): List[Int] = {
		val distTo = new mutable.HashMap[Xy, Double]()
		val edgeTo = new mutable.HashMap[Xy, Xy]()
		
		def relax(from: Xy, to: Xy): Unit = {
			if (!distTo.contains(to) || distTo(to) > distTo(from) + energies(from)) {
				distTo(to) = distTo(from) + energies(from)
				edgeTo(to) = from
			}
		}
		
		distTo(Xy(-1, -1)) = 0.0
		val s = new Stopwatch()
		for (p <- topological; v <- adj(p))
			relax(p, v)
		
		println(s"Relaxed after ${s.elapsedTime()}")
		
		pathTo(edgeTo.toMap, selector)
	}
	
	
	private def pathTo(map: Map[Xy, Xy], select: Xy => Int): List[Int] = {
		val path = new mutable.ArrayStack[Int]()
		var edge = map(Xy(width, height))
		while (map.contains(edge)) {
			path push select(edge)
			edge = map(edge)
		}
		path.toList
	}
	
	private def hAdj(p: Xy): Seq[Xy] = p match {
		case a if a == first => (0 until width).map(x => Xy(x, 0))
		case a if a == last => Seq.empty[Xy]
		case a if a.y == height - 1 => Seq(last)
		case _ => Seq(p.downLeft, p.down, p.downRight).filter(i => i.inRange(width, height))
	}
	
	private def vAdj(p: Xy): Seq[Xy] = p match {
		case a if a == first => (0 until height).map(y => Xy(0, y))
		case a if a == last => Seq.empty[Xy]
		case a if a.x == width - 1 => Seq(last)
		case _ => Seq(p.upRight, p.right, p.downRight).filter(i => i.inRange(width, height))
	}
	
	def removeHorizontalSeam(seam: List[Int]): Unit = {
		require(seam != null, "seam can not be null")
		require(seam.length == width, s"invalid seam length: ${seam.length} == $width")
		require(seam.nonEmpty, "seam can not be empty")
		require(validate(seam, height), "seam has invalid jump or out-of-range value")
		require(height > 1, "picture height most be greater than 1")
		
		val newPic = new Picture(width, height - 1)
		for (y <- 0 until height; x <- 0 until width) {
			val _y = if (y >= seam(x)) y - 1 else y
			newPic.setRGB(x, _y, picture.getRGB(x, y))
		}
		set(newPic)
		
	}
	
	def removeVerticalSeam(seam: List[Int]): Unit = {
		require(seam != null)
		require(seam.length == height, s"invalid seam length: ${seam.length} == $height")
		require(seam.nonEmpty, "seam can not be empty")
		require(validate(seam, width), "seam has invalid jump or out-of-range value")
		require(width > 1, "Picture width most be greater than 1")
		
		val newPic = new Picture(width - 1, height)
		for (x <- 0 until width; y <- 0 until height) {
			val _x = if (x >= seam(y)) x - 1 else x
			newPic.setRGB(_x, y, picture.getRGB(x, y))
		}
		set(newPic)
	}
	
	
	private def validate(seam: List[Int], bound: Int): Boolean = {
		@tailrec
		def loop(prev: Int, list: List[Int]): Boolean = list match {
			case seam if seam.isEmpty => 0 <= prev && prev < bound
			case seam if (0 > prev || prev >= bound) || math.abs(prev - seam.head) > 1 => false
			case _ => loop(list.head, list.tail)
		}
		
		loop(seam.head, seam.tail)
	}
	
	case class Xy(x: Int, y: Int) {
		override def toString: String = s"($x, $y)"
		
		def upRight: Xy = Xy(x + 1, y - 1)
		
		def right: Xy = Xy(x + 1, y)
		
		def downRight: Xy = Xy(x + 1, y + 1)
		
		def down: Xy = Xy(x, y + 1)
		
		def downLeft: Xy = Xy(x - 1, y + 1)
		
		def inRange(width: Int, height: Int): Boolean = 0 <= x && x < width && 0 <= y && y < height
		
		def ==(t: (Int, Int)): Boolean = x == t._1 && y == t._2
	}
	
	class EnergyMatrix(Picture: Picture) {
		val height: Int = picture.height()
		val width: Int = picture.width()
		
		private val matrix: Array[Array[Double]] = Array.fill[Double](height, width)(0.0)
		private val xOff = Xy(1, 0)
		private val yOff = Xy(0, 1)
		
		val sw = new Stopwatch()
		for (x <- 0 until width; y <- 0 until height)
			matrix(y)(x) = energy(Xy(x, y))
		println(s"Build energy matrix after ${sw.elapsedTime()}")
		
		@inline def apply(p: Xy): Double =
			if (p.x == -1 && p.y == -1 || (p.x == width && p.y == height)) 0
			else matrix(p.y)(p.x)
		
		@inline def update(p: Xy, value: Double): Unit = matrix(p.y)(p.x) = value
		
		def energy(p: Xy): Double = {
			require(0 <= p.x && p.x < width, s"invalid x: 0 <= ${p.x} < $width")
			require(0 <= p.y && p.y < height, s"invalid y: 0 <= ${p.y} < $height")
			if (isABorderPixel(p)) return 999.00
			math.sqrt(Diff(p, red, xOff) +
				Diff(p, green, xOff) +
				Diff(p, blue, xOff) +
				Diff(p, red, yOff) +
				Diff(p, green, yOff) +
				Diff(p, blue, yOff))
		}
		
		def verticalTopOrder: Seq[Xy] = {
			val marked = mutable.HashSet[Xy]()
			val reversPost = mutable.ArrayStack[Xy]()
			
			dfs(first)
			for (x <- 0 until width; y <- 0 until height) {
				val p = Xy(x, y)
				if (!marked.contains(p)) dfs(p)
			}
			
			def dfs(p: Xy): Unit = {
				marked += p
				for (v <- hAdj(p))
					if (!marked.contains(v)) dfs(v)
				reversPost push p
			}
			
			reversPost
		}
		
		def horizontalTopOrder: Seq[Xy] = {
			val sw = new Stopwatch()
			val marked = mutable.HashSet[Xy]()
			val reversPost = mutable.ArrayStack[Xy]()
			
			dfs(first)
			for (y <- 0 until height; x <- 0 until width) {
				val p = Xy(x, y)
				if (!marked.contains(p)) dfs(p)
			}
			
			def dfs(p: Xy): Unit = {
				marked += p
				for (v <- vAdj(p))
					if (!marked.contains(v)) dfs(v)
				reversPost push p
			}
			
			println(s"Ordered after ${sw.elapsedTime()}")
			reversPost
		}
		
		@inline
		private def Diff(p: Xy, color: Int => Int, offset: Xy): Double =
			math.pow(color(picture.getRGB(p.x - offset.x, p.y - offset.y))
				- color(picture.getRGB(p.x + offset.x, p.y + offset.y)), 2)
		
		@inline private def isABorderPixel(p: Xy) = p.x == 0 || p.y == 0 || p.x == width - 1 || p.y == height - 1
		
		private def red(color: Int): Int = (color & 0xff0000) >> 16
		
		private def green(color: Int): Int = (color & 0xff00) >> 8
		
		private def blue(color: Int): Int = color & 0xff
		
		override def toString: String = {
			val builder = new StringBuilder()
			for (x <- 0 until width) {
				builder append '\n'
				for (y <- 0 until height) {
					val energy = matrix(y)(x)
					builder append (if (energy == 0.0) "     0 " else "%1.2f".format(energy) + " ")
				}
			}
			builder.toString()
		}
	}
	
}

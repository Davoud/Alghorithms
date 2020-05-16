package graphAssignments

import edu.princeton.cs.algs4.{Picture, Stopwatch}
import dataStructures.graphs.{EdgeWeightedDigraph, Topological}

object SeamCarverTest {
	
	def test1(): Unit = {
		val file = ".\\src\\main\\scala\\graphAssignments\\test2.jpg"
		val pic = new Picture(file)
		println(s"Image ${pic.width()}x${pic.height()}")
		val carver = new SeamCarver(pic)
		for (i <- 0 until 150) {
			carver.removeHorizontalSeam(carver.findHorizontalSeam)
			println(s"Passed (H) phase $i")
		}
		carver.picture.save(".\\src\\main\\scala\\graphAssignments\\test_next_h.jpg")
		println("saved")
	}
	
	def performance(): Unit = {
		// needed -Xss18m
		val file = ".\\src\\main\\scala\\graphAssignments\\test1.jpg"
		val pic = new Picture(file)
		println(s"Image ${pic.width()}x${pic.height()}")
		val sw0 = new Stopwatch()
		val carver = new SeamCarver(pic)
		val sw1 = new Stopwatch()
		var hSeam = carver.findHorizontalSeam
		println(s"Found H-Seam after ${sw1.elapsedTime()} seconds")
		val sw2 = new Stopwatch()
		carver.removeHorizontalSeam(hSeam)
		println(s"Removed H-Seam after ${sw2.elapsedTime()} seconds")
		println(s"Finished after ${sw0.elapsedTime()} seconds")
	}
	
	
	def testTop(): Unit = {
		var g = new EdgeWeightedDigraph(8)
		g += ((0, 1, 0.0), (0, 2, 0.0), (1, 3, 6.0), (1, 4, 2.0), (2, 3, 3.0), (2, 4, 4.0), (3, 5, 0.0), (4, 5, 0.0))
		g += ((0, 6, 0.0), (6, 3, .9), (6, 7, .3), (1, 7, 3.3), (7, 0, .0))
		for (v <- new Topological(g).order)
			println(v)
	}
	
	
}

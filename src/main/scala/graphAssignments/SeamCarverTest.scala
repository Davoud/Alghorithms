package graphAssignments

import edu.princeton.cs.algs4.Picture
import dataStructures.graphs.{EdgeWeightedDigraph, Topological}

object SeamCarverTest {
	
	def test1(): Unit = {
		
		//val file = ".\\src\\main\\scala\\graphAssignments\\sample.bmp"
		val file = ".\\src\\main\\scala\\graphAssignments\\bout_40w.jpg"
		val pic = new Picture(file)
		println(s"Image ${pic.width()}x${pic.height()}")
		
		val carver = new SeamCarver(pic)
		//println(carver.energies.inTopologicalOrder)
		val vSeam = carver.findVerticalSeam
		println(s"Vertical Length: ${vSeam.length}")
		println(vSeam)
		
		val hSeam = carver.findHorizontalSeam
		println(s"Horizontal Length ${hSeam.length}")
		println(hSeam)
		
		
	}
	
	def testTop(): Unit = {
		var g = new EdgeWeightedDigraph(8)
		g += ((0, 1, 0.0), (0, 2, 0.0), (1, 3, 6.0), (1, 4, 2.0), (2, 3, 3.0), (2, 4, 4.0), (3, 5, 0.0), (4, 5, 0.0))
		g += ((0, 6, 0.0), (6, 3, .9), (6, 7, .3), (1, 7, 3.3), (7, 0, .0))
		for (v <- new Topological(g).order)
			println(v)
	}
	
	
}

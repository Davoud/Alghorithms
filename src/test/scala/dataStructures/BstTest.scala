package dataStructures

class BstTest {
	
	private def sampleValues: Array[Int] = {
		val array = new Array[Int](26)
		for(i <- 0 until 26)
			array(i) = i + 65
		sorting.Sorting.shuffle(array)
		array
	}
	
	
}

package strings

import scala.reflect.ClassTag

case class Item[K](val value: K, val key: Int);

object KeyIndexCounting {
	
	def sort[K: ClassTag](R: Int)(a: Array[Item[K]]): Array[K] = {
		
		val N = a.length;
		val aux = new Array[K](N)
		val count = new Array[Int](R + 1)
		
		for (i <- 0 until N)
			count(a(i).key + 1) += 1
		
		for (r <- 0 until R)
			count(r + 1) += count(r)
		
		for (i <- 0 until N) {
			val k = a(i).key
			aux(count(k)) = a(i).value
			count(k) += 1
		}
		
		aux
	}
}
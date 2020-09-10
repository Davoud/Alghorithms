package strings


class MSD {
	
	type Text = Array[String]
	
	private val R = 256
	private val M = 3
	
	var aux: Text = Array.empty[String]
	
	def charAt(s: String, d: Int): Int = if (d < s.length) s(d) else -1
	
	def sort(a: Text): Unit = {
		val N = a.length
		aux = new Array[String](N)
		sort(a, 0, N - 1, 0)
	}
	
	private def sort(a: Text, lo: Int, hi: Int, d: Int): Unit = {
		
		if (hi <= lo + M) {
			insertion(a, lo, hi, d)
			return Unit
		}
		
		val count = new Array[Int](R + 2)
		
		for (i <- lo to hi)
			count(charAt(a(i), d) + 2) += 1
		
		for (r <- 0 until R + 1)
			count(r + 1) += count(r)
		
		for (i <- lo to hi) {
			val k = charAt(a(i), d) + 1
			aux(count(k)) = a(i)
			count(k) += 1
		}
		
		for (i <- lo to hi)
			a(i) = aux(i - lo)
		
		for (r <- 0 until R)
			sort(a, lo + count(r), lo + count(r + 1) - 1, d + 1)
	}
	
	
	def insertion(a: Text, lo: Int, hi: Int, d: Int): Unit = {
		for (i <- lo to hi) {
			var j = i
			while (j > lo && less(a(j), a(j - 1), d)) {
				swap(a, j, j - 1)
				j -= 1
			}
		}
	}
	
	@inline
	private def swap(a: Text, v: Int, w: Int): Unit = {
		val t = a(v)
		a(v) = a(w)
		a(w) = t
	}
	
	@inline
	private def less(v: String, w: String, d: Int): Boolean = v.substring(d).compareTo(w.substring(d)) < 0
	
	private def dump(c: Array[Int]): Unit = {
		var b = new StringBuilder()
		for (i <- c.indices) {
			if (c(i) > 0)
				b = b.append(s"$i:${c(i)} ")
		}
		println(b.toString())
	}
	
}


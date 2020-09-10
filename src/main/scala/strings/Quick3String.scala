package strings

class Quick3String(private val a: Array[String]) {
	
	sort(0, a.length - 1, 0)
	
	def Result: Array[String] = a
	
	private def sort(lo: Int, hi: Int, d: Int): Unit = {
		
		if (hi <= lo) return Unit
		
		var lt = lo
		var gt = hi
		val v = charAt(a(lo), d)
		var i = lo + 1
		
		while (i <= gt) {
			val t = charAt(a(i), d)
			if (t < v) {
				swap(lt, i)
				lt += 1
				i += 1
			}
			else if (t > v) {
				swap(i, gt)
				gt -= 1
			}
			else {
				i += 1
			}
		}
		
		sort(lo, lt - 1, d)
		if (v >= 0)
			sort(lt, gt, d + 1)
		sort(gt + 1, hi, d)
	}
	
	@inline private def swap(v: Int, w: Int): Unit = {
		val t = a(v)
		a(v) = a(w)
		a(w) = t
	}
	
	@inline private def charAt(s: String, d: Int): Int = if (d < s.length) s(d) else -1
}

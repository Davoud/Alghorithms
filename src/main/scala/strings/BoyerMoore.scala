package strings

class BoyerMoore(val pattern: String, alphabet: Alphabet) extends StringFinder {
	require(alphabet.Verify(pattern))
	private val M = pattern.length
	private val R = alphabet.R
	private val right = Array.fill(R)(-1)
	for (j <- 0 until M) right(alphabet(pattern(j))) = j
	
	override def search(text: String): Int = {
		require(alphabet.Verify(text))
		
		val N = text.length
		var i = 0
		
		while (i <= N - M) {
			var j = M - 1
			var skip: Option[Int] = None
			while (j >= 0 && (skip.isEmpty || skip.get > 1)) {
				if (pattern(j) != text(i + j)) {
					skip = Some(j - right(alphabet(text(i + j))))
					if (skip.get < 1) skip = Some(1)
				}
				if (skip.isDefined && skip.get == 0) return i
				j -= 1
			}
			i += 1
		}
		
		N
	}
}

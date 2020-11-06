package strings

class KMP(val pattern: String) extends StringFinder {
	
	private val M = pattern.length
	private val R = 256
	private val dfa = computeDfa()
	
	override def search(text: String): Int = {
		val N = text.length
		var j = 0
		var i = 0
		while (i < N && j < M) {
			j = dfa(text(i))(j)
			i += 1
		}
		if (j == M) i - M else N
	}
	
	private def computeDfa(): Array[Array[Int]] = {
		val dfa = Array.fill(R, M)(0)
		var X = 0
		var j = 1
		dfa(pattern(0))(0) = 1
		while (j < M) {
			for (c <- 0 until R) dfa(c)(j) = dfa(c)(X)
			dfa(pattern(j))(j) = j + 1
			X = dfa(pattern(j))(X)
			j += 1
		}
		dfa
	}
}

class KMPAlphabet(pattern: String, alphabet: Alphabet) extends StringFinder {
	require(alphabet.Verify(pattern))
	
	private val M = pattern.length
	private val R = alphabet.R
	val dfa = computeDfa()
	
	override def search(text: String): Int = {
		require(alphabet.Verify(text))
		val N = text.length
		var j = 0
		var i = 0
		while (i < N && j < M) {
			j = dfa(alphabet(text(i)))(j)
			i += 1
		}
		if (j == M) i - M else N
	}
	
	private def computeDfa(): Array[Array[Int]] = {
		
		val dfa = Array.fill(R, M)(0)
		var X = 0
		var j = 1
		dfa(alphabet(pattern(0)))(0) = 1
		while (j < M) {
			for (c <- 0 until R) dfa(c)(j) = dfa(c)(X)
			val index = alphabet(pattern(j))
			dfa(index)(j) = j + 1
			X = dfa(index)(X)
			j += 1
		}
		dfa
	}
	
	
	
	
}
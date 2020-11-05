package strings

class Alphabet(private val letters: String) {
	
	
	private val chars = letters.toCharArray
	private val indices = chars.zipWithIndex.toMap
	
	def R: Int = chars.size
	
	def lgR: Int = Math.ceil(Math.log10(chars.size) / Math.log10(2)).toInt
	
	def contains(c: Char): Boolean = chars.contains(c)
	
	def toChar(index: Int): Char = chars(index)
	
	def toIndex(c: Char): Int = indices(c)
	
	def toIndices(s: String): Array[Int] = {
		val arr = Array.fill(s.length)(-1)
		
		for ((c, i) <- s.zipWithIndex)
			if (indices.contains(c))
				arr(i) = indices(c)
		
		arr.filter(i => i != -1)
	}
	
	def toChars(indices: Seq[Int]): String = {
		val builder = new StringBuilder()
		for (i <- indices if i < chars.length)
			builder.append(chars(i))
		builder.toString()
	}
	
	def Verify(text: String): Boolean = text.forall(indices.contains)
	
	override def toString: String = letters
	
	@inline def apply(index: Int): Char = toChar(index)
	
	@inline def apply(c: Char): Int = toIndex(c)
}

package strings

import org.scalatest.{FlatSpec, Matchers}

class AlphabetTest extends FlatSpec with Matchers {
	
	val genome = new Alphabet("ACGT")
	
	"Alphabet(ACGT)" should "return R and lgR as 5 and 3 resp." in {
		genome.R should be(4)
		genome.lgR should be(2)
	}
	
	it should "gives indices and chars as (A: 0, C: 1, G: 2: T: 3)" in {
		genome('T') should be(3)
		genome('G') should be(2)
		genome('C') should be(1)
		genome('A') should be(0)
	}
	
	it should "mat indexed to chars as (A: 0, C: 1, G: 2: T: 3)" in {
		genome(0) should be('A')
		genome(1) should be('C')
		genome(2) should be('G')
		genome(3) should be('T')
	}
	
	it should "specify other chars as invalid" in {
		genome.contains('A') should be(true)
		genome.contains('C') should be(true)
		genome.contains('G') should be(true)
		genome.contains('T') should be(true)
		genome.contains('a') should be(false)
		genome.contains('b') should be(false)
		genome.contains('c') should be(false)
		genome.contains('g') should be(false)
	}
	
	it should "converts arbitrary text to indices" in {
		genome.toIndices("AACGACTTG").toSeq should be(Seq(0, 0, 1, 2, 0, 1, 3, 3, 2))
	}
	
	it should "converts arbitrary text to indices ignoring invalid letters" in {
		genome.toIndices("A.AaCbbG__AC TTG").toSeq should be(Seq(0, 0, 1, 2, 0, 1, 3, 3, 2))
	}
	
	it should "converts back to string with indice " in {
		genome.toChars(Seq(0, 0, 1, 2, 0, 1, 3, 3, 2)) should be("AACGACTTG")
	}
}

package strings

import org.scalatest.{FlatSpec, Matchers}

class SearchTests extends FlatSpec with Matchers {
	
	val text = "The code that we have considered is a compact and complete implementation " +
		"of the string symbol-table API that has broadly useful practical applications Several variations " +
		"and extensions are discussed in the exercises. Next, we consider basic properties of tries and some " +
		"limitations on their utility The code that we have considered is a compact and complete implementation " +
		"discussed in the exercises. Next, we consider basic properties of limitations on their utility The code " +
		"that we have considered is a compact "
	
	"KMP" should "find substring " in {
		val kmp = new KMP("The code")
		kmp.search(text) should be(0)
	}
	
	"KMP" should "find substring (in middle)" in {
		val pattern = "properties of limit"
		val kmp = new KMP(pattern)
		kmp.search(text) should be(text.indexOf(pattern))
	}
	
	"KMP" should "not find substring (in middle)" in {
		val pattern = "not in the text"
		val kmp = new KMP(pattern)
		kmp.search(text) should be(text.length)
	}
	
	"KMP (ABC)" should "find the pattern" in {
		var kmp = new KMPAlphabet("ABABAC", new Alphabet("ABC"))
		kmp.search("BCBAABACAABABACAA") should be(9)
	}
	
	"BoyerMoor (ABC)" should "find the pattern" in {
		var bm = new BoyerMoore("ABABAC", new Alphabet("ABC"))
		bm.search("BCBAABACAABABACAA") should be(9)
	}
	
}

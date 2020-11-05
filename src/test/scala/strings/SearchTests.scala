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
	
	"2 Dim Array" should "work as expected" in {
		val a = Array.fill(256, 4)(0)
		val s = "1234"
		a(s(1))(0) = 10
		println(a(s(1))(0))
	}
}

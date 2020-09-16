package strings

import org.scalatest.{FlatSpec, Matchers}

class TriesTest extends FlatSpec with Matchers {
	
	"TriesST" should "get single item" in {
		val t = new TriesST[Int]()
		t("a") = 1
		t("a") should be(Some(1))
		t("b") should be(None)
	}
	
	it should "behave as it expected" in {
		val t = new TriesST[String]()
		
		t("abc") = "abc"
		t("a") = "a"
		t("abcd") = "abcd"
		t("h") = "h"
		
		t("h") should be(Some("h"))
		t("a") should be(Some("a"))
		t("abcd") should be(Some("abcd"))
		t("abc") should be(Some("abc"))
		
	}
	
	"Option(null)" should "be None" in {
		Option(null) should be(None)
	}
	
	"Match length '1234'" should "be 4" in {
		val k = "1234"
		val d = 3
		val m = k.length match {
			case l if (l == d) => true
			case _ => false
		}
		
		m should be(false)
		
	}
}

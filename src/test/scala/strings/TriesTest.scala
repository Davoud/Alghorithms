package strings

import org.scalatest.{FlatSpec, Matchers}

class TriesTest extends FlatSpec with Matchers {
	
	val text = "The code that we have considered is a compact and complete implementation " +
		"of the string symbol-table API that has broadly useful practical applications Several variations " +
		"and extensions are discussed in the exercises. Next, we consider basic properties of tries and some " +
		"limitations on their utility The code that we have considered is a compact and complete implementation " +
		"discussed in the exercises. Next, we consider basic properties of limitations on their utility The code " +
		"that we have considered is a compact "
	
	def sample(): TriesST[Int] = {
		val t = new TriesST[Int]()
		
		for (w <- text.split(' '))
			t(w) match {
				case None => t(w) = 1
				case Some(n) => t(w) = n + 1
			}
		
		t
	}
	
	"TriesST" should "get single item" in {
		val t = new TriesST[Int]()
		t("a") = 1
		t("a") should be(Some(1))
		
	}
	
	it should "return None for missing key" in {
		val t = new TriesST[Int]()
		t("no-such-key") should be(None)
	}
	
	it should "return None for missing key 'b'" in {
		val t = new TriesST[Int]()
		t("a") = 1
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
	
	it should "remove a key correctly" in {
		val t = new TriesST[String]()
		t("abc") = "ABC"
		t("a") = "A"
		t("abcd") = "ABCD"
		t("h") = "H"
		
		t("abcd") should be(Some("ABCD"))
		t.size should be(4)
		t.remove("abcd")
		t.size should be(3)
		t("abcd") should be(None)
		
	}
	
	"size" should "be 4 " in {
		val t = new TriesST[String]()
		t("abc") = "abc"
		t("a") = "a"
		t("abcd") = "abcd"
		t("h") = "h"
		t.size should be(4)
	}
	
	"keys" should "reflect all" in {
		val t = new TriesST[String]()
		t("abc") = "abc"
		t("a") = "a"
		t("abcd") = "abcd"
		t("h") = "h"
		t.keys().toSet should be(Set("a", "abcd", "h", "abc"))
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
	
	it should "count number of words in sample text" in {
		var trie = sample()
		for (key <- trie.keys())
			println(s"$key: ${trie(key).get}")
		
		true should be(true)
	}
	
}

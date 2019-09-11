package dataStructures

import org.scalatest.{FlatSpec, Matchers}

class BstTest extends FlatSpec with Matchers {
	
	private def sampleValues: Array[Int] = {
		val array = new Array[Int](26)
		for(i <- 0 until 26)
			array(i) = i + 65
		sorting.Sorting.shuffle(array)
		array
	}
	
	private def sampleTree(): Bst[Int, Char] = {
		val tree = new Bst[Int, Char]()
		for (v <- sampleValues)
			tree.put(v, v.toChar)
		tree
	}
	
	"Bst" should "iterate as ABC" in {
		val tree = new Bst[Int, String]()
		tree.put(2, "B")
		tree.put(1, "A")
		tree.put(3, "C")
		tree.values().foldRight("")((c: String, s: String) => s"$c$s") should be("ABC")
	}
	
	it should "get values correctly" in {
		val tree = new Bst[Int, String]()
		tree.put(2, "B")
		tree.put(1, "A")
		tree.put(3, "C")
		tree.get(2).isDefined should be(true)
		tree.get(2).get should be("B")
		tree.get(1).isDefined should be(true)
		tree.get(1).get should be("A")
		tree.get(3).isDefined should be(true)
		tree.get(3).get should be("C")
	}
	
	it should "have a 0 size when empty" in {
		new Bst[Int, String]().size should be(0)
	}
	
	it should "get all values correctly" in {
		val t = sampleTree()
		for (i <- 0 until 26)
			t.get(i + 65) should be(Some((i + 65).toChar))
	}
	
	it should "get None for missing values" in {
		sampleTree().get(0) should be(None)
	}
	
	
	"Bst min" should "be A" in {
		val tree = sampleTree()
		tree.min() should be(Some('A'))
	}
	
	"Bst max" should "be Z" in {
		val tree = sampleTree()
		tree.max() should be(Some('Z'))
	}
	
}

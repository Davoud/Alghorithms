package dataStructures

import dataStructures.trees.Bst
import org.scalatest.{FlatSpec, Matchers}

class BstTest extends FlatSpec with Matchers {
	
	private def sampleValues: Array[Int] = {
		val array = new Array[Int](26)
		for(i <- 0 until 26)
			array(i) = i + 65
		sorting.Sorting.shuffle(array)
		array
	}
	
	private def sampleTree(len: Int = 26, gap: Int = 1): Bst[Int, Char] = {
		val tree = new Bst[Int, Char]()
		for (v <- sampleChars(len, gap))
			tree.put(v, v)
		tree
	}
	
	private def sampleChars(length: Int = 26, gap: Int = 1): Array[Char] = {
		val list = new scala.collection.mutable.ListBuffer[Char]
		val cc = 'A'
		(0 until length by gap).foreach(i => list += (cc + i).toChar)
		val array = list.toArray
		sorting.Sorting.shuffle(array)
		array
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
		for (i <- 'A' to 'Z')
			t.get(i) should be(Some(i))
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
	
	"Bst rank of A" should "be 0" in {
		sampleTree().rank('A') should be(0)
	}
	
	"Bst ranks of A..F" should "be 0..5" in {
		val tree = sampleTree()
		var rank = 0
		for (v <- 'A' to 'F') {
			tree.rank(v) should be(rank)
			rank += 1
		}
	}
	
}

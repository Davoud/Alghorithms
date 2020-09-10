package strings

import org.scalatest.{FlatSpec, Matchers}

class KeyIndexCountTest extends FlatSpec with Matchers {
	
	def sampleOne(): Array[Item[String]] = {
		val s = new Array[Item[String]](100)
		for (i <- s.indices)
			s(i) = Item(s"$i", i % 10)
		s
	}
	
	def sampleTwo(): Array[String] = {
		val s = "A second pitfall for MSD string sort is that  it can be relatively slow for subarrays containing large  numbers of equal keys. If a substring occurs sufficiently  often that the cutoff for small subarrays does not apply,  then a recursive call is needed for every character  in all of the equal keys. Moreover, key-indexed counting  is an inefficient way to determine that the characters  are all equal: not only does each character need to  be examined and each string moved, but all the counts  have to be initialized, converted to indices, and so forth.  Thus, the worst case for MSD string sorting is when all  keys are equal. The same problem arises when large numbers  of keys have long common prefixes, a situation often  found in applications."
		s.split(' ')
	}
	
	def sampleThree(): Array[String] = {
		val s = "she sells seashells by the sea shore the shells she sells are surely seashells"
		s.split(' ')
	}
	
	"Key index counting " should "sort array sample one" in {
		
		val a: Array[String] = KeyIndexCounting.sort(10)(sampleOne())
		for (w <- 0 until a.length - 1)
			println(a(w))
		
	}
	
	"1st and 2nd chars of '4P14XZ' " should "be '4' and 'P'" in {
		val str = "4P14XZ"
		str(0) should be('4')
		str(1) should be('P')
		"KRM"(2) should be('M')
	}
	
	"Insertion sort" should "sort array of strings properly " in {
		var a = sampleTwo()
		val msd = new MSD()
		msd.insertion(a, 0, a.length - 1, 0)
		for (w <- 0 until a.length - 1)
			(a(w) <= a(w + 1)) should be(true)
	}
	
	"Array " should " update via indexer" in {
		val c = new Array[Int](10)
		c(0) = 1
		c(1) = 1
		c(2) += 3
		c(2) += 10
		println(c.fold(">")((a, b) => s"$a $b"))
	}
	
	"CharAt " should "return meaningful values" in {
		val s = "she sells seashells by the sea shore the shells she sells are surely seashells z Z 9"
		val msd = new MSD()
		var max = Int.MinValue
		var min = Int.MaxValue
		for (i <- 0 to s.length - 1) {
			val v = msd.charAt(s, i)
			if (v >= max) max = v
			if (v <= min) min = v
		}
		
		println(s"Min: $min (${min.asInstanceOf[Char]}), Max $max (${max.asInstanceOf[Char]})");
		
	}
	
	"MSD sort" should "sort array of string properly" in {
		var a = sampleThree()
		val msd = new MSD()
		msd.sort(a)
		for (w <- 0 until a.length - 1) {
			//println (a(w))
			(a(w) <= a(w + 1)) should be(true)
		}
	}
}

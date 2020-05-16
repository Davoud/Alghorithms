package wordNet

import graphAssignments.Synsets
import org.scalatest.{FlatSpec, Matchers}

class SynsetTest extends FlatSpec with Matchers {
	
	private val file = ".\\src\\test\\scala\\wordNet\\sampleSyns.txt"
	
	"Synset length of sample" should "be 23" in {
		val synset = new Synsets(file)
		synset.length should be(23)
	}
	
	"Synset id of '1530s'" should "be 1" in {
		val synset = new Synsets(file)
		synset("1530s") should be(1)
	}
}

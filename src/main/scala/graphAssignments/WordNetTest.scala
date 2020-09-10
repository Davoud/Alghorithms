package graphAssignments

object WordNetTest {
	def Test(): Unit = {
		val synsets = ".\\src\\main\\scala\\graphAssignments\\synsets.txt"
		val hypernyms = ".\\src\\main\\scala\\graphAssignments\\hypernyms.txt"
		val wordNet = new WordNet(synsets, hypernyms)
		println(wordNet.nouns.foldLeft("")((a, s) => s"$a, $s"))
	}
}



object Main extends App {
	val m = scala.collection.mutable.Map[Int, String]()
	
	m(1) = "one"
	m(2) = "two"
	m(1) = "ONE"
	m += (3 -> "Three")
	m += (1 -> "Oh boy")
	for (k <- m)
		println(k)
}


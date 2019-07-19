import edu.princeton.cs.algs4.Stopwatch
import sorting.Sorting

object Main extends App {

  TestMerge()

  def TestMerge(): Unit = {

    val a = new Array[Int](10000)
    a.indices.foreach(i => a(i) = i + 1)
    Sorting.shuffle(a)
    val s = new Stopwatch()
    Sorting.merge(a)
    println(s.elapsedTime())

  }

  def prt[T](a: Array[T]): Unit = {
    println(a.foldRight("")((a, b) => s"$a $b"))
  }

}


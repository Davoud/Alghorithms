package sorting

import edu.princeton.cs.algs4.StdRandom


object Sorting{

  private def exch[K](a: Array[K], i: Int, j: Int): Unit = {
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }

  def insertion[K: Ordering](a: Array[K]): Unit = {
    val o = implicitly[Ordering[K]]
    for (i <- a.indices) {
      for(j <- i until 0 by -1)
        if (o.lt(a(j), a(j - 1))) exch(a, j, j - 1)
    }
  }

  def selection[K: Ordering](a: Array[K]): Unit = {
    val o = implicitly[Ordering[K]]
    for(i <- a.indices) {
      var min = i
      for (j <- i + 1 until a.length)
        if(o.lt(a(j), a(min))) min = j
      exch(a, i, min)
    }
  }

  def shell[K: Ordering](a: Array[K]): Unit = {
    val o = implicitly[Ordering[K]]
    var h = 1
    while(h < a.length/3) h = 3*h + 1
    while(h >= 1) {
      for(i <- h until a.length){
        var j = i
        while(j >= h && o.lt(a(j), a(j - h))) {
          exch(a, j, j - h)
          j -= h
        }
      }
      h = h / 3
    }
  }

  def shuffle[K](a: Array[K]): Unit =
    a.indices.foreach(i => exch(a, i, StdRandom.uniform(i + 1)))

}






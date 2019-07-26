package sorting

import edu.princeton.cs.algs4.{StdRandom, Stopwatch}


object Sorting{

  private def exch[K](a: Array[K], i: Int, j: Int): Unit = {
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }
  
  def isSorted[K](a: Array[K])(implicit o: Ordering[K]): Boolean = {
    for (i <- 0 until a.length - 1)
      if (o.gt(a(i), a(i + 1))) return false
    
    true
  }
  
  def insertion[K](a: Array[K], ordering: Ordering[K]): Unit = {
    for (i <- a.indices) {
      for(j <- i until 0 by -1)
        if (ordering.lt(a(j), a(j - 1))) exch(a, j, j - 1)
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


  def merge[K](a: Array[K])(implicit ev: Manifest[K], ord: Ordering[K]): Unit = {
    val aux = new Array[K](a.length)
    sort(a, aux, 0, a.length - 1)
  }

  private def merge[K: Ordering](a: Array[K], aux: Array[K], lo: Int, mid: Int, hi: Int): Unit = {
    val o = implicitly[Ordering[K]]

    for (k <- lo to hi)
      aux(k) = a(k)

    var i = lo
    var j = mid + 1

    for (k <- lo to hi) {
      if (i > mid) {
        a(k) = aux(j); j += 1
      }
      else if (j > hi) {
        a(k) = aux(i); i += 1
      }
      else if (o.lt(aux(j), aux(i))) {
        a(k) = aux(j); j += 1
      }
      else {
        a(k) = aux(i); i += 1
      }
    }

  }

  private def sort[K: Ordering](a: Array[K], aux: Array[K], lo: Int, hi: Int): Unit = {
    if (hi <= lo) return
    val mid = lo + (hi - lo) / 2
    sort(a, aux, lo, mid)
    sort(a, aux, mid + 1, hi)
    merge(a, aux, lo, mid, hi)
  }
  
  def quick[K](a: Array[K])(implicit o: Ordering[K]): Unit = {
    shuffle(a)
    quick(a, 0, a.length - 1)
  }
  
  def printState[K](a: Array[K], index: Int, lo: Int, hi: Int): Unit = {
    val str = a.foldRight("")((a, b) => s"$a $b")
    println(s"[$str]\tp: $index, lo: $lo, hi: $hi")
  }
  
  def quick[K](a: Array[K], lo: Int, hi: Int)(implicit o: Ordering[K]): Unit = {
    if (hi <= lo) return
    val index = partition(a, lo, hi)
    //printState(a, index, lo, hi)
    quick(a, lo, index - 1)
    quick(a, index + 1, hi)
  }
  
  private def partition[K](a: Array[K], lo: Int, hi: Int)(implicit o: Ordering[K]): Int = {
    var i = lo + 1
    var j = hi
    
    while (i < j) {
      while (i < hi && o.lt(a(i), a(lo))) i += 1
      while (j > lo && o.lt(a(lo), a(j))) j -= 1
      if (i < j) exch(a, i, j)
    }
    
    exch(a, lo, j)
    j
  }
  
  object SortingMethod extends Enumeration {
    val Selection = Value(1)
    val Insertion = Value(2)
    val Shell = Value(3)
    val Merge = Value(4)
    val Quick = Value(5)
  }
  
  def TestSort(method: SortingMethod.Value, sampleSize: Int = 100000) = {
    
    val sample = new Array[Long](sampleSize)
    
    for (i <- sample.indices)
      sample(i) = i
    
    sorting.Sorting.shuffle(sample)
    println(s"IsSorted: ${sorting.Sorting.isSorted(sample)}")
    
    val t = new Stopwatch()
    
    method match {
      case SortingMethod.Selection => sorting.Sorting.selection(sample)
      case SortingMethod.Insertion => sorting.Sorting.insertion(sample, implicitly[Ordering[Long]])
      case SortingMethod.Shell => sorting.Sorting.shell(sample)
      case SortingMethod.Merge => sorting.Sorting.merge(sample)
      case SortingMethod.Quick => sorting.Sorting.quick(sample)
    }
    
    println(s"Time ${t.elapsedTime()}")
    println(s"IsSorted: ${sorting.Sorting.isSorted(sample)}")
    
  }
}






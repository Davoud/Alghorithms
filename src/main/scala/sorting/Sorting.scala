package sorting

import edu.princeton.cs.algs4.{StdRandom, Stopwatch}


object Sorting{
  
  @inline
  private def exch[K](a: Array[K], i: Int, j: Int, offset: Int = 0): Unit = {
  
    if (a.length < 2 || i == j)
      return
    
    val ii = i + offset
    val jj = j + offset
    
    val temp = a(ii)
    a(ii) = a(jj)
    a(jj) = temp
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
  
  
  def quick[K](a: Array[K], lo: Int, hi: Int)(implicit o: Ordering[K]): Unit = {
    if (hi <= lo) return
    val index = partition2(a, lo, hi)
    quick(a, lo, index - 1)
    quick(a, index + 1, hi)
  }
  
  private def partition[K](a: Array[K], lo: Int, hi: Int)(implicit o: Ordering[K]): Int = {
    var i = lo + 1
    var j = hi
  
    while (i <= j) {
      while (i <= hi && o.lt(a(i), a(lo))) i += 1
      while (j >= lo && o.lt(a(lo), a(j))) j -= 1
      if (i < j) exch(a, i, j)
    }
    
    exch(a, lo, j)
    j
  }
  
  private def partition2[K](collection: Array[K], leftMostIndex: Int, rightMostIndex: Int)(implicit comparer: Ordering[K]): Int = {
    
    var wallIndex: Int = leftMostIndex
    val pivotValue: K = collection(rightMostIndex)
    
    for (i <- leftMostIndex until rightMostIndex) {
      if (comparer.lt(collection(i), pivotValue)) {
        exch(collection, i, wallIndex)
        wallIndex += 1
      }
    }
    
    exch(collection, wallIndex, rightMostIndex)
    wallIndex
  }
  
  def quick3Way[K](collection: Array[K])(implicit comparer: Ordering[K]): Unit = {
    quick3Way(collection, 0, collection.length - 1)
  }
  
  
  private def quick3Way[K](collection: Array[K], leftMostIndex: Int, rightMostIndex: Int)
                          (implicit comparer: Ordering[K]): Unit = {
    if (rightMostIndex <= leftMostIndex) return
    var lt = leftMostIndex
    var gt = rightMostIndex
    val value = collection(leftMostIndex)
    
    var index = leftMostIndex
    while (index <= gt) {
      val cmp = comparer.compare(collection(index), value)
      if (cmp < 0) {
        exch(collection, lt, index)
        lt += 1
        index += 1
      }
      else if (cmp > 0) {
        exch(collection, index, gt)
        gt -= 1
      }
      else {
        index += 1
      }
    }
    
    quick3Way(collection, leftMostIndex, lt - 1)
    quick3Way(collection, gt + 1, rightMostIndex)
  }
  
  def heap[K](collection: Array[K])(implicit ordering: Ordering[K]): Unit = {
    
    var N = collection.length
    for (k <- N / 2 to 1 by -1)
      sink(collection, k, N)
    
    while (N > 1) {
      exch(collection, 1, N, -1)
      N -= 1
      sink(collection, 1, N)
    }
    
  }
  
  private def sink[K: Ordering](collection: Array[K], index: Int, N: Int): Unit = {
    var k = index
    while (2 * k <= N) {
      var j = 2 * k
      if (j < N && less(collection, j, j + 1, -1)) j += 1
      if (!less(collection, k, j, -1)) return
      exch(collection, k, j, -1)
      k = j
    }
  }
  
  private def less[K: Ordering](collection: Array[K], i: Int, j: Int, offset: Int = 0): Boolean =
    implicitly(Ordering[K]).lt(collection(i + offset), collection(j + offset))
  
  
  object SortingMethod extends Enumeration {
    val Selection = Value(1)
    val Insertion = Value(2)
    val Shell = Value(3)
    val Merge = Value(4)
    val Quick = Value(5)
    val Quick3Way = Value(6)
    val Heap = Value(7)
  }
  
  def TestSort(method: SortingMethod.Value, sampleSize: Int = 100000, duplications: Option[Int] = None) = {
    
    val sample = new Array[Long](sampleSize)
    
    for (i <- sample.indices)
      sample(i) = if (duplications.isDefined) i % duplications.get else i
    
    sorting.Sorting.shuffle(sample)
    
    val t = new Stopwatch()
    
    method match {
      case SortingMethod.Selection => sorting.Sorting.selection(sample)
      case SortingMethod.Insertion => sorting.Sorting.insertion(sample, implicitly[Ordering[Long]])
      case SortingMethod.Shell => sorting.Sorting.shell(sample)
      case SortingMethod.Merge => sorting.Sorting.merge(sample)
      case SortingMethod.Quick => sorting.Sorting.quick(sample)
      case SortingMethod.Quick3Way => sorting.Sorting.quick3Way(sample)
      case SortingMethod.Heap => sorting.Sorting.heap(sample)
    }
    
    println(s"${method} Finished in ${t.elapsedTime()} for a sample of size $sampleSize")
    
    if (!sorting.Sorting.isSorted(sample)) {
      for (i <- sample) print(s"$i ")
      throw new Exception("Sample has not been sorted properly!")
    }
    
  }
  
  
}




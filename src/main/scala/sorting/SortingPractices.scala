package sorting

import sorting.Color.Color

case class Point2D(x: Double, y: Double) extends Ordered[Point2D] {
  override def compare(that: Point2D): Int = {
    if(x < that.x) return -1
    if(x > that.x) return 1
    if(y < that.y) return -1
    if(y > that.y) return 1
    0
  }
}

object Color extends Enumeration {
  type Color = Value
  val Red, White, Blue = Value
}

class PointIntersect {

  def numberOfEqualPoints(a: Array[Point2D], b: Array[Point2D]): Int = {
    Sorting.shell(a)
    Sorting.shell(b)
    var count = 0
    for(i <- 0 until Math.min(a.length, b.length))
    {
      if(a(i) == b(i))
        count += 1
    }
    count
  }

  def permutationCheck(a: Array[Int], b: Array[Int]): Boolean = {
    Sorting.shell(a)
    Sorting.shell(b)
    var count = 0
    for(i <- a.indices){
      if(a(i) == b(i))
        count += 1
    }
    count == a.length
  }



  case class Bucket(var color: Color)

  def swap(a: Array[Bucket], i: Int, j: Int): Unit = {
    val c = a(i).color
    a(i).color = a(j).color
    a(j).color = c
  }

  def color(a: Array[Bucket], i: Int): Color = a(i).color


  def orderPebbles(a: Array[Bucket]): Unit = {

  }

}

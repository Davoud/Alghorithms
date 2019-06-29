import dataStructures.{ArrayQueue, LinkedListQueue}
import edu.princeton.cs.algs4.{StdRandom, Stopwatch}
import percolation.{ConnectionManager, PercolationStats}
import quickfind._
import sorting._

object Main extends App {

   TestSorting()

   def TestSorting(): Unit = {


      val x = Array(Person("Mike"), Person("Alison"), Person("Jack"), Person("Bernie"))
      val length = 10000
      val y = new Array[Int](length)

      (0 until length).foreach(i => y(i) = length - i)

      //Sorting.shuffle(y)

      Sorting.shell(x)
      val stopwatch = new Stopwatch()
      //Sorting.selection(y)
      //Sorting.shell(y)
      Sorting.insertion(y)
      println(stopwatch.elapsedTime())
      //scala.util.Sorting.quickSort(y)

      println(x.foldRight("")((a, b) => s"$a, $b"))
      //println(y.foldRight("")((a, b) => s"$a, $b"))





   }

   //TestArrayQueue2()

   case class Person(name: String) extends Ordered[Person] {
      override def toString: String = name
      override def compare(that: Person): Int = name.compareTo(that.name)
   }

   def TestArrayQueue2(): Unit = {
      val q = new ArrayQueue[Person](null)
      println("Enqueue")
      for (i <- 1 to 8) {
         q.enqueue(Person("P" + i))
         //println(s"  L: ${q.length()}, S: ${q.queueSize()}, Item: ${i}")
         println(q)
      }

      for (_ <- 1 to 5)
      {
         val item = q.dequeue()
         //println(s"  L: ${q.length()}, S: ${q.queueSize()}, Item: ${item}")
         println(s"$q [$item]")

      }

      for(i <- 9 to 15)
      {
         q.enqueue(Person(s"P$i"))
         //println(s"  L: ${q.length()}, S: ${q.queueSize()}, Item: ${i}")
         println(q)
      }

      for(p <- q)
         println(p)

      while(!q.isQueueEmpty)
      {
         val item = q.dequeue()
         println(s"$q [$item]")
      }


   }

   def TestArrayQueue(): Unit = {
      val q = new ArrayQueue[Int](-1)

      println("Enqueue")
      for (i <- 1 to 8) {
         q.enqueue(i)
         //println(s"  L: ${q.length()}, S: ${q.queueSize()}, Item: ${i}")
         println(q)
      }

      for (_ <- 1 to 5)
      {
         val item = q.dequeue()
         //println(s"  L: ${q.length()}, S: ${q.queueSize()}, Item: ${item}")
         println(s"$q [$item]")

      }

      for(i <- 9 to 15)
      {
         q.enqueue(i)
         //println(s"  L: ${q.length()}, S: ${q.queueSize()}, Item: ${i}")
         println(q)
      }

      while(!q.isQueueEmpty)
      {
         val item = q.dequeue()
         println(s"$q [$item]")
      }


   }



   def TestLinkListQueue(): Unit =
   {
      val queue = new LinkedListQueue[Int]()
      queue.enqueue(1)
      queue.enqueue(2)
      queue.enqueue(3)
      queue.enqueue(4)
      queue.enqueue(5)

      for(item <- queue)
         println(item)

   }



   def TestPercolationStats(n: Int, t: Int): Unit = {
      val stopwatch = new Stopwatch()
      val ps = new PercolationStats(n, t)
      println(s"elapsed time = ${stopwatch.elapsedTime()}")
      println(s"mean = ${ps.mean()}")
      println(s"stddev = ${ps.stddev()}")
      println(s"95% confidence interval = [${ps.confidenceLo()}, ${ps.confidenceHi()}]\n")
   }

   def TestStdRandom(): Unit =
   {

      for(n <- 1 to 10)
         println(StdRandom.uniform(10) + " " + StdRandom.uniform(10))
   }

   def TestConnectionManager(): Unit = {
      val cm = new ConnectionManager(2, 2, (_,_) => true)
      cm.open(0, 0)
      cm.open(1, 0)
      Console.println(cm.percolates())
   }

   def TestQuickUnion()
   {
      //var uf = new QuickFindUF(5)
      //var uf = new QuickUnion(5)
      var uf = new QuickFindBalanced(5)
      println("Init: " + uf)
   
      uf.union(0, 1)
      println("union (0, 1): " + uf)
      //uf.union(1, 2)
      //println(uf)
   
      uf.union(2, 3)
      println("union (2, 3): " + uf)
   
      uf.union(0, 4)
      println("union (0, 4): " + uf)
      
      println("connnected (0, 3): " + uf.connected (0, 3))
      println("connnected (1, 4): " + uf.connected (1, 4))

      println(uf)
   }
}


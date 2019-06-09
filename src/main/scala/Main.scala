import edu.princeton.cs.algs4.{StdRandom, Stopwatch}
import percolation.{ConnectionManager, PercolationStats}
import quickfind._

object Main extends App {




   def TestAllConnecte4(): Unit =
   {
      val q = new QuickFindBalanced(5)



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


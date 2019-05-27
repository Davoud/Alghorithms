package percolation

import edu.princeton.cs.algs4.StdRandom
import edu.princeton.cs.algs4.StdStats


class PercolationStats(n: Int, trails: Int) {

  if(n <= 0 || trails <= 0)
    throw new IllegalArgumentException()

  val openFraction: Array[Double] = new Array[Double](trails)

  for(t <- 0 until trails) {
    val p = new Percolation(n)

    while(!p.percolates())
      p.open(StdRandom.uniform(n), StdRandom.uniform(n))

    openFraction(t) = p.numberOfOpenSites().toDouble / (n * n)
  }

  def mean(): Double = StdStats.mean(openFraction)
  def stddev(): Double = StdStats.stddev(openFraction)
  def confidenceLo(): Double = mean - (1.96 * stddev / Math.sqrt(trails))
  def confidenceHi(): Double = mean + (1.96 * stddev / Math.sqrt(trails))
}

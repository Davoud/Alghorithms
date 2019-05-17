package percolation

import org.scalatest.FunSuite
import percolation._

class PercolationTest extends FunSuite
{
    test("All nodes are closed at initiation") {
        val n = 5
        val p = new Percolation(n)
        for(row <- 0 until n)
            for(col <- 0 until n)
                assert(p.isOpen(row, col) === false)
    }

    test(testName = "Opening Nodes") {
        val n = 5
        val p = new Percolation(n)
        p.open(0, 0)
        assert(p.isOpen(0, 0) === true)
    }

    test("Open All Nodes") {
        val n = 100
        val p = new Percolation(n)

        for(row <- 0 until n)
            for(col <- 0 until n)
                p.open(row, col)

        for(row <- 0 until n)
            for(col <- 0 until n)
                assert(p.isOpen(row, col) === true)
    }

    test ("Is Fill") {
        val n = 5
        val p = new Percolation(n)
        assert(p.isFull(0, 0) === false)
        p.isOpen(0, 0)
        assert(p.isFull(0, 0) === true)
    }

}
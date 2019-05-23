package percolation

import org.scalatest.FunSuite


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

    test("Percolates 1x1") {
        val n = 1
        val p = new Percolation(n)
        assert(p.percolates() === false)
        p.open(0,0)
        assert(p.percolates() === true)
    }

    test("Percolates 2x2 route 10/10") {
        val n = 2
        val p = new Percolation(n)
        assert(p.percolates() === false)
        p.open(0, 0)
        assert(p.percolates() === false)
        p.open(1, 0)
        assert(p.percolates() === true)
    }

    test("Not percolates 2x2 route 10/01") {
        val n = 2
        val p = new Percolation(n)
        p.open(0, 0)
        assert(p.percolates() === false)
        p.open(1,1)
        assert(p.percolates() === false)
    }

    test("Percolates 10x10 route: middle vertial line"){
        val n = 10
        val p = new Percolation(n)

        assert(p.percolates() === false)

        for(row <- 0 until n)
            p.open(row, 5)

        assert(p.percolates() === true)
    }

    test("Not percolates 10x10 diagonal") {
        val n = 10
        val p = new Percolation(n)

        assert(p.percolates() === false)

        for(i <- 0 until n)
            p.open(i, i)

        assert(p.percolates() === false)
    }

    test("Number of open sites 10x10 diagonal") {
        val n = 10
        val p = new Percolation(n)

        assert(p.numberOfOpenSites() === 0)

        for(i <- 0 until n) {
            p.open(i, i)
            assert(p.numberOfOpenSites() === i+1)
        }

        assert(p.numberOfOpenSites() === 10)
    }
}
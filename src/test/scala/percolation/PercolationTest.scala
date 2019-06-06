import org.scalatest.FunSuite
import percolation._

class PercolationTest extends FunSuite
{
    test("All nodes are closed at initiation") {
        var n = 5;
        var p = new Percolation(n)
        for(row <- 0 to n - 1)
        {
            for(col <- 0 to n - 1)
            {
                assert(p.isOpen(row, col) === false)
            }
        }
    }

    
}
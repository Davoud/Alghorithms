package percolation

import org.scalatest.FunSuite
import quickfind.QuickFindBalanced

class ConnectionManagerTest extends FunSuite {

     test("percolate 1x1 matrix") {
        val cm = new ConnectionManager(1, 1, (_, _) => true)
        assert(cm.percolates() === false)
        cm.open(0, 0)
        assert(cm.percolates() === true)
     }

    test("percolatex 2x2 matrix, route 10/10"){
      var cm = new ConnectionManager(2,2, (_,_) => true)
      assert(cm.percolates() === false)
      cm.open(0,0)
      assert(cm.percolates() === false)
      cm.open(1, 0)
      assert(cm.percolates() === true)
    }

    test("percolate 2x2 matrix, route 01/01"){
      val cm = new ConnectionManager(2, 2, (_, _) => true)
      assert(cm.percolates() === false)
      cm.open(0,1)
      assert(cm.percolates() === false)
      cm.open(1, 1)
      assert(cm.percolates() === true)
    }

    test("not percolate 2x2 diagonal") {
      val cm = new ConnectionManager(2, 2, (a, b) => a == b)
      assert(cm.percolates() === false)
      cm.open(0,0)
      assert(cm.percolates() === false)
      cm.open(1, 1)
      assert(cm.percolates() === false)
    }

  test("not percolate 2x2 reverse diagonal") {
    val cm = new ConnectionManager(2, 2, (a, b) => a != b)
    assert(cm.percolates() === false)
    cm.open(0,1)
    assert(cm.percolates() === false)
    cm.open(1, 0)
    assert(cm.percolates() === false)
  }

  test("percolates 3x3 route 100/100/100"){
    val cm = new ConnectionManager(3, 3, (a, b) => b == 0)
    assert(cm.percolates() === false)
    cm.open(0 ,0)
    assert(cm.percolates() === false)
    cm.open(1, 0)
    assert(cm.percolates() === false)
    cm.open(2, 0)
    assert(cm.percolates() === true)
  }

  test("All connected"){

    val q = new QuickFindBalanced(5)
    assert(q.allConnected() === false)

    q.union(0, 1)
    assert(q.allConnected() === false)

    q.union(1, 2)
    assert(q.allConnected() === false)

    q.union(2, 3)
    assert(q.allConnected() === false)

    q.union(3, 4)
    assert(q.allConnected() === true)

  }
}

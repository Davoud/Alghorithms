package percolation

import org.scalatest.FunSuite

class ConnectionManagerTest extends FunSuite {

     test("Open a node") {
        val cm = new ConnectionManager(5, 5, (_, _) => true)
        assert(cm.isOpen(0, 0) === false)
        cm.open(0, 0)
        assert(cm.isOpen(0, 0) === true)

     }
}

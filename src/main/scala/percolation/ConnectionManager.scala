package percolation

import quickfind._

case class Position(val row: Int, val column: Int)

class ConnectionManager(val rows: Int, val columns: Int, val percolation: Percolation) {

    val headIndex = 0
    val tailIndex = (rows * columns) + 1
    val quickFind = new QuickFindBalanced(tailIndex + 1)
    val head = new Position(0, -1)
    val tail = new Position(rows-1, columns)
    
    def open(row: Int, col: Int) = {
        var opened = new Position(row, col)
        var indexOfOpened = indexOf(opened)
        for(p <- openNeighboursOf(opened))
        {
            quickFind.union(indexOf(p), indexOfOpened)
        }
    }
    
    def percolates() = connected(head, tail)

    def isFull(row: Int, col: Int) = connected(head, new Position(row, col))

    def connected(p1: Position, p2: Position) = quickFind.connected(indexOf(p1), indexOf(p2))
    
    private def indexOf(position: Position) : Int = position.row * columns + position.column + 1
   
    private def openNeighboursOf(p: Position): Seq[Position] = {
        val neighbours = Vector(
            Position(p.row - 1, p.column),
            Position(p.row + 1, p.column),
            Position(p.row, p.column - 1),
            Position(p.row, p.column + 1))

        neighbours
    }
} 
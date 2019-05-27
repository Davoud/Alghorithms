package percolation

import edu.princeton.cs.algs4.WeightedQuickUnionUF
import edu.princeton.cs.algs4.QuickFindUF
import edu.princeton.cs.algs4.QuickUnionUF
import quickfind._

case class Position(row: Int, column: Int)

class ConnectionManager(val rows: Int, val columns: Int, var isOpen: (Int, Int) => Boolean) {

    val headIndex = 0
    val tailIndex: Int = (rows * columns) + 1
    //val quickFind = new WeightedQuickUnionUF(tailIndex + 1)
    val quickFind = new QuickFindBalanced(tailIndex + 1)
    //val quickFind = new QuickFindUF(tailIndex + 1)
    //val quickFind = new QuickUnionUF(tailIndex + 1)
    val head = Position(0, -1)
    val tail = Position(rows - 1, columns)
    
    def open(row: Int, col: Int): Unit = {
        val opened = Position(row, col)
        val indexOfOpened = indexOf(opened)
        for(p <- openNeighboursOf(opened))
            quickFind.union(indexOf(p), indexOfOpened)
    }
    
    def percolates(): Boolean = connected(head, tail)

    def isFull(row: Int, col: Int): Boolean = connected(head, Position(row, col))

    def connected(p1: Position, p2: Position): Boolean = quickFind.connected(indexOf(p1), indexOf(p2))
    
    private def  indexOf(position: Position) : Int = position.row * columns + position.column + 1
   
    private def openNeighboursOf(p: Position): Seq[Position] = {
        var neighbours = Vector(
            Position(p.row - 1, p.column),
            Position(p.row + 1, p.column),
            Position(p.row, p.column - 1),
            Position(p.row, p.column + 1))

        if(p.row == 0 && !neighbours.contains(head))
            neighbours = neighbours :+ head

        if(p.row == rows - 1 && !neighbours.contains(tail))
            neighbours = neighbours :+ tail

        neighbours.filter(validAndOpen)
    }

    private def validAndOpen(p: Position) : Boolean = {
        if(p == head) return true
        if(p == tail) return true
        if(p.row >= 0 && p.column >= 0 && 
           p.row < rows && p.column < columns &&
           isOpen(p.row, p.column)) return true

        false
    }
} 

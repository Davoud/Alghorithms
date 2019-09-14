package dataStructures

import scala.collection.mutable
import scala.collection.mutable.Map



class Bst[Key, Value](implicit ordering: Ordering[Key]){

    private var root: Option[Node] = None
    
    def put(key: Key, value: Value): Unit = {
        root = put(root, key, value)
    }

    private def put(x: Option[Node], key: Key, value: Value): Option[Node] = {
        if(x.isEmpty) return Some(Node(key, value))
    
        val node = x.get
        val cmp = ordering.compare(key, node.key)
        if(cmp < 0)
            node.left = put(node.left, key, value)
        else if (cmp > 0)
            node.right = put(node.right, key, value)
        else
            node.value = value
    
        node.count = 1 + size(node.left) + size(node.right)
        x
    }
    
    private def size(x: Option[Node]): Int = if (x.isDefined) x.get.count else 0
    
    def size: Int = size(root)
    
    def get(key: Key): Option[Value] = {
        val node = getNode(key)
        if (node.isDefined) Some(node.get.value) else None
    }
    
    private def getNode(key: Key): Option[Node] = {
        var x: Option[Node] = root
        while (x.isDefined) {
            val cmp = ordering.compare(key, x.get.key)
            if (cmp < 0) x = x.get.left
            else if (cmp > 0) x = x.get.right
            else return x
        }
        None
    }
    
    def min(): Option[Value] = if(root.isEmpty) None else Some(min(root.get).value)
    
    private def min(x: Node): Node = {
        var node = x
        while (node.left.isDefined)
            node = node.left.get
        node
    }
    
    private def max(x: Node): Node = {
        var node = x
        while (node.right.isDefined)
            node = node.right.get
        node
    }
    
    def max(): Option[Value] = if (root.isEmpty) None else Some(max(root.get).value)
    
    def rank(key: Key): Int = rank(key, root)
    
    private def rank(key: Key, node: Option[Node]): Int = {
        if (node.isEmpty) return 0
        
        val cmp = ordering.compare(key, node.get.key)
        if (cmp < 0)
            rank(key, node.get.left)
        else if (cmp > 0)
            1 + size(node.get.left) + rank(key, node.get.right)
        else
            size(node.get.left)
    }
    
    def floor(key: Key): Option[Key] = {
        val x = floor(key, root)
        if (x.isEmpty) None
        else Some(x.get.key)
    }
    
    private def floor(key: Key, x: Option[Node]): Option[Node] = {
        if (x.isEmpty) return None
        
        val cmp = ordering.compare(key, x.get.key)
        
        if (cmp == 0) return x
        if (cmp < 0) return floor(key, x.get.left)
        
        val t = floor(key, x.get.right)
        if (t.isDefined) t else x
    }
    
    def ceiling(key: Key): Option[Key] = {
        val x = ceiling(key, root)
        if (x.isEmpty) None else Some(x.get.key)
    }
    
    private def ceiling(key: Key, x: Option[Node]): Option[Node] = {
        if (x.isEmpty) return None
        val cmp = ordering.compare(key, x.get.key)
        
        if (cmp == 0) return x
        if (cmp > 0) return ceiling(key, x.get.right)
        
        val t = ceiling(key, x.get.left)
        if (t.isDefined) t else x
    }
    
    def delete(key: Key): Unit = root = delete(key, root)
    
    private def delete(key: Key, x: Option[Node]): Option[Node] = {
        if (x.isEmpty) return None
        
        var node = x.get
        val cmp = ordering.compare(key, node.key)
        if (cmp < 0)
            node.left = delete(key, node.left)
        else if (cmp > 0)
            node.right = delete(key, node.right)
        else {
            if (node.left.isEmpty) return node.right
            if (node.right.isEmpty) return node.left
            
            val t = node
            node = min(t.right.get)
            node.right = deleteMin(t.right.get)
            node.left = t.left
        }
        node.count = size(node.left) + size(node.right) + 1
        x
    }
    
    private def deleteMin(x: Node): Option[Node] = {
        if (x.left.isEmpty) return x.right
        x.left = deleteMin(x.left.get)
        x.count = 1 + size(x.left) + size(x.right)
        Some(x)
    }
	  
	  def keys(traversMode: TraversMode.Value = TraversMode.PreOrder): Iterable[Key] =
        traverse(traversMode).map(node => node.key)
    
    def values(traversMode: TraversMode.Value = TraversMode.PreOrder): Iterable[Value] =
        traverse(traversMode).map(node => node.value)
    
    private def traverse(traversMode: TraversMode.Value): ArrayQueue[Node] = {
        val queue = new ArrayQueue[Node]()
        traversMode match
        {
            case TraversMode.PreOrder => preOrder(root, queue)
            case TraversMode.InOrder => inOrder(root, queue)
            case TraversMode.PostOrder => postOrder(root, queue)
        }
        queue
    }
    
    private def preOrder(x: Option[Node], queue: ArrayQueue[Node]): Unit = {
         if(x.isEmpty) return
         preOrder(x.get.left, queue)
         queue.enqueue(x.get)
         preOrder(x.get.right, queue)
    }
    
    private def inOrder(x: Option[Node], queue: ArrayQueue[Node]): Unit = {
        if(x.isEmpty) return
        queue.enqueue(x.get)
        inOrder(x.get.left, queue)
        inOrder(x.get.right, queue)
    }
    
    private def postOrder(x: Option[Node], queue: ArrayQueue[Node]): Unit = {
        if(x.isEmpty) return
        postOrder(x.get.left, queue)
        postOrder(x.get.right, queue)
        queue.enqueue(x.get)
    }
    
    private case class Node
        (key: Key,
         var value: Value,
         var left: Option[Node] = None,
         var right: Option[Node] = None,
         var count: Int = 0)
    
    type NodesInfo = mutable.Map[(Int, Int), NodeInfo]
    
    def nodes(key: Option[Key] = None): NodesInfo = {
        val map: NodesInfo = mutable.Map()
        
        if (key.isDefined) {
            val node = getNode(key.get)
            if (node.isDefined)
                fill(map, 0, "0", node)
        }
        else
            fill(map, 0, "0", root)
        
        map
    }
    
    def depth(key: Option[Key] = None): Int = {
        if (key.isDefined) {
            val node = getNode(key.get)
            if (node.isDefined)
                node.get.count
            else
                0
        }
        else if (root.isDefined)
            root.get.count
        else
            0
    }
    
    //    private def depth(node: Option[Node], d: Int): Int = {
    //        if(node.isEmpty) return d
    //
    //        var left = -1
    //        var right = -1
    //
    //        if(node.get.left.isDefined)
    //            left = node.get.left.get.count
    //
    //        if(node.get.right.isDefined)
    //            right = node.get.right.get.count
    //
    //        if(left == -1 && right == -1)
    //            return d + 1
    //
    //        if(left > right)
    //            depth(node.get.left, d + 1)
    //        else if(left >= right)
    //            depth(node.get.right, d + 1)
    //        else
    //            d
    //
    //    }
    
    private def fill(map: NodesInfo, level: Int, identifier: String, node: Option[Node]): Unit = {
        if (node.isEmpty) return
        
        val n = node.get
        
        map((level, Integer.parseInt(identifier, 2))) = NodeInfo(n.value.toString, n.left.isDefined, n.right.isDefined)
        
        if (n.left.isDefined)
            fill(map, level + 1, identifier + "0", n.left)
        
        if (n.right.isDefined)
            fill(map, level + 1, identifier + "1", n.right)
    }
    
}


object TraversMode extends Enumeration {
    val InOrder = Value(1)
    val PreOrder = Value(2)
    val PostOrder = Value(3)
}

case class NodeInfo(value: String, hasLeftChild: Boolean, hasRightChild: Boolean, IsRed: Boolean = false)

object BinarySearchTreeTest {
    
    def Test1() = {
        
        val tree = new Bst[Int, Char]()
        for (v <- sampleValues)
            tree.put(v, v.toChar)
        
        println(tree.values(TraversMode.InOrder).foldRight("")((c: Char, s: String) => s"$c $s"))
        println(tree.values(TraversMode.PreOrder).foldRight("")((c: Char, s: String) => s"$c $s"))
        println(tree.values(TraversMode.PostOrder).foldRight("")((c: Char, s: String) => s"$c $s"))
        
    }
    
    def Test2(): Unit = {
        val t = new Bst[Int, Char]()
        t.put(5, 'E')
        println(t.size)
        print(t)
        t.put(1, 'A')
        println(t.size)
        print(t)
        t.put(10, 'I')
        println(t.size)
        print(t)
    }
    
    def Test3(): Unit = {
        val tree = new Bst[Int, Char]()
        val visualizer = new TreeVisualizer(tree)
        for (i <- 1 to 10) {
            println(i)
            visualizer.print()
            println()
        }
    }
    
    def Test(): Unit = {
        val tree = new Bst[Int, Char]()
        
        //        tree.put(4, 'D')
        //        tree.put(2, 'B')
        //        tree.put(6, 'F')
        //        tree.put(1, 'A')
        //        tree.put(3, 'C')
        //        tree.put(5, 'E')
        //        tree.put(7, 'G')
        //        tree.put(8, 'H')
        //        tree.put(9, 'I')
        
        
        for (v <- sampleValues)
            tree.put(v, v.toChar)
        
        val nodes = tree.nodes()
        for (n <- nodes)
            println(s"${n._1} = ${n._2.value}")
        
        val vis = new TreeVisualizer(tree)
        vis.print()
    }
    
    private def sampleValues: Array[Int] = {
        val array = new Array[Int](7)
        for (i <- 0 until 7)
            array(i) = i + 65
        sorting.Sorting.shuffle(array)
        array
    }
    
    def print[Key, Value](bst: Bst[Key, Value]): Unit = {
        val values = bst.values(TraversMode.InOrder)
        println(values.foldRight("")((v,s) => s"$v $s"))
    }
    
    
}

class TreeVisualizer[Key, Value](tree: Bst[Key, Value]) {
    
    def print(): Unit = {
        
        var levelSize = 1
        var level = 0
        var padLen = Math.pow(2, tree.depth()) - 1
        val nodes = tree.nodes()
        
        while (padLen >= 1) {
            val line: StringBuilder = new StringBuilder
            for (i <- 0 until levelSize) {
                val node = nodes.get((level, i))
                val value = if (node.isDefined) node.get.value else " "
                line.append(pad(value, padLen.toInt)).append(" ")
            }
            println(line)
            levelSize *= 2
            padLen /= 2
            level += 1
        }
    }
    
    private def pad(value: String, len: Int): String = {
        val pad = spaces((len - value.length) / 2)
        s"$pad$value$pad"
    }
    
    private def spaces(len: Int): String = {
        var str = ""
        for (_ <- 0 until len)
            str += " "
        str
    }
}
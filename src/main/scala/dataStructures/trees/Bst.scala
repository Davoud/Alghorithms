package dataStructures.trees

import dataStructures.queues.ArrayQueue

import scala.collection.mutable



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
    
    
    private def fill(map: NodesInfo, level: Int, identifier: String, node: Option[Node]): Unit = {
        if (node.isEmpty) return
        
        val n = node.get
        
        map((level, Integer.parseInt(identifier, 2))) = NodeInfo(n.value.toString,
            childInfo(n.left),
            childInfo(n.right))
        
        if (n.left.isDefined)
            fill(map, level + 1, identifier + "0", n.left)
        
        if (n.right.isDefined)
            fill(map, level + 1, identifier + "1", n.right)
    }
    
    private def childInfo(node: Option[Node]): ChildInfo.Value = {
        if (node.isEmpty)
            ChildInfo.None
        else
            ChildInfo.Black
        
    }
}



object TraversMode extends Enumeration {
    val InOrder = Value(1)
    val PreOrder = Value(2)
    val PostOrder = Value(3)
}

object ChildInfo extends Enumeration {
    val None = Value(0)
    val Black = Value(1)
    val Red = Value(2)
}

case class NodeInfo(value: String, left: ChildInfo.Value = ChildInfo.None, right: ChildInfo.Value = ChildInfo.None)


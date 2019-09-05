import dataStructures.ArrayQueue

class Bst[Key, Value](implicit ordering: Ordering[Key]){

    private var root: Option[Node] = None
    
    def put(key: Key, value: Value): Unit = {
        root = put(root, key, value)
    }

    private def put(x: Option[Node], key: Key, value: Value): Option[Node] = {
        if(x.isEmpty) return Some(Node(key, value))
        
        val cmp = ordering.compare(key, x.get.key)
        if(cmp < 0)
            x.get.left = put(x.get.left, key, value)
        else if (cmp > 0)
            x.get.right = put(x.get.right, key, value)
        else
            x.get.value = value
        
        x
    }
    
    def get(key: Key): Option[Value] = {
        var x: Option[Node] = root
        while(x.isDefined) {
            val cmp = ordering.compare(key, x.get.key)
            if (cmp < 0) x = x.get.left
            else if (cmp > 0) x = x.get.right
            else return Some(x.get.value)
        }
        return None
    }

    def min(): Option[Value] = {
        if(root.isEmpty) return None
        var x = root
        while(x.isDefined)
            if(x.get.left.isDefined)
                x = x.get.left
        Some(x.get.value)
    }
    
    def max(): Option[Value] = {
        if(root.isEmpty) return None
        var x = root
        while(x.isDefined)
            if(x.get.right.isDefined)
                x = x.get.right
        Some(x.get.value)
    }
    def delete(key: Key): Unit = ???

    def keys(): Iterator[Key] = {
        val q = new ArrayQueue[Option[Key]](None)
        traverse(root, q)
        q.filter(k => k.isDefined).map(k => k.get)
    }

     private def traverse(x: Option[Node], queue: ArrayQueue[Option[Key]]): Unit = {
         if(x.isEmpty) return
         traverse(x.get.left, queue)
         queue.enqueue(Some(x.get.key))
         traverse(x.get.right, queue)
     }

    private case class Node
        (key: Key,
         var value: Value,
         var left: Option[Node] = None,
         var right: Option[Node] = None,
         var size: Int = 0)
}
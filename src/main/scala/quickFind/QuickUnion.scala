package quickfind

class QuickUnion(N: Int)
{
    private var id = new Array[Int](N);

    for(i <- 0 to id.length - 1)
        id(i) = i

    def root(p: Int) : Int = {        
        var i = p
        while(i != id(i)) i = id(i)        
        i
    }

    def union(p: Int, q: Int) = id(root(p)) = root(q)       

    def connected(p: Int, q: Int) : Boolean = root(p) == root(q)

    override def toString = id.deep.mkString(",")
}
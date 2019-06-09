package quickfind

class QuickFindBalanced(N: Int)
{
    private val id = new Array[Int](N)
    private val size = new Array[Int](N)

    for(i <- 0 until id.length)
    {
        id(i) = i
        size(i) = 1
    }
    
    
    def root(p: Int) : Int = {        
        var i = p
        while(i != id(i)) { 
            id(i) = id(id(i))
            i = id(i)        
        }
        i
    }

    private var hasASingleTree: Boolean = false

    def allConnected() : Boolean = hasASingleTree

    def union(p: Int, q: Int) = {
        var i = root(p)
        var j = root(q)               
        if(i != j) {        
            if(size(i) < size(j)) {
                id(i) = j
                size(j) += size(i)
                hasASingleTree = size(j) == N
            }
            else {
                id(j) = i
                size(i) += size(j)
                hasASingleTree = size(i) == N
            }        
        }
    }


    def connected(p: Int, q: Int) : Boolean = root(p) == root(q)

    override def toString = id.deep.mkString(",")       

}
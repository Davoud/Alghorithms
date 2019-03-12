package quickFind

class QuickFindBalanced(N: Int)
{
    private val id = new Array[Int](N)
    private val size = new Array[Int](N)

    for(i <- 0 to id.length - 1) 
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

    def union(p: Int, q: Int) = {
        var i = root(p)
        var j = root(q)               
        if(i != j) {        
            if(size(i) < size(j)) {
                id(i) = j
                size(j) += size(i)
            }
            else {
                id(j) = i
                size(i) += size(j)
            }        
        }
    }


    def connected(p: Int, q: Int) : Boolean = root(p) == root(q)

    override def toString = id.deep.mkString(",")       

}
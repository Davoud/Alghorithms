package quickFind

class QuickFindBalanced(N: Int)
{
    private val id = new Array[Int](N)
    private val sizes = scala.collection.immutable.Map[Int,Int]()

    for(i <- 0 to id.length - 1)
        id(i) = i
    
    def root(p: Int) : Int = {        
        var i = p
        while(i != id(i)) i = id(i)        
        i
    }

    def union(p: Int, q: Int) = {
        var rootp = root(p)
        var rootq = root(q)
        
        var sizep = size(p)
        var sizeq = size(q)

        if(sizep > sizeq)
        {
                 
        }
        else
        {

        }

        id(rootp) = root(q)       
    }


    def connected(p: Int, q: Int) : Boolean = root(p) == root(q)

    override def toString = id.deep.mkString(",")
    
    def size(p: Int): Int = sizes.getOrElse(p, 1)    

}
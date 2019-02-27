package quickfind

class QuickFindUF(N: Int)
{
    private var id = new Array[Int](N);

    for(i <- 0 to id.length - 1)
        id(i) = i

    def connected(p: Int, q: Int) : Boolean = id(p) == id(q)
    
    def union(p: Int, q: Int) =
    {
        val pid = id(p)
        val qid = id(q)
        for(i <- 0 to id.length - 1)
            if(id(i) == pid) id(i) = qid
    }
}
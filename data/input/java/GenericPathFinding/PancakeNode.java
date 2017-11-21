import java.util.Random;

public class PancakeNode extends SearchNode{

    protected int[] id;
    public static boolean cacheEdges = true;

    public PancakeNode(){
        this.parent = null;
        this.id = null;
        this.edges = null;
    }

    public PancakeNode(int i){
        super(i);
        this.id = new int[i];
        for(int j = 0; j < i; j++){
            this.id[j] = j;
        }
        this.edges = null;
    }

    public static PancakeNode flip(PancakeNode src, int pivot){
        if(pivot == 0) return src;
        else{
            PancakeNode next = new PancakeNode(src.id.length);
            next.id = new int[src.id.length];
            for(int i = 0; i < pivot; i++){
                next.id[pivot - i - 1] = src.id[i];
            }
            for(int i = pivot; i < src.id.length; i++){
                next.id[i] = src.id[i];
            }
            return next;
        }
    }

    public static PancakeNode cannonicalGoal(int size){
        return new PancakeNode(size);
    }

    public static PancakeNode randomInstance(int size){
        PancakeNode toRet = new PancakeNode(size);
        Random r = new Random();
        for(int i = 0; i < 500; i++){
            toRet = flip(toRet, r.nextInt(size - 1));
        }
        return toRet;
    }

    public Node[] adjacent(){
        Edge[] e = this.getEdges();
        //System.out.println(e.length);
        PancakeNode[] adj = new PancakeNode[e.length];
        for(int i = 0; i < e.length; i++){
            adj[i] = (PancakeNode)e[i].getDestination();
            if(adj[i] != this) adj[i].parent = this;
        }
        return adj;
    }

    public Edge[] getEdges(){
        if (this.edges != null){
            return this.edges;
        }else{
            Edge[] e = new Edge[this.id.length];
            for(int i = 0; i < this.id.length; i++){
                Node succ = PancakeNode.flip(this, i);
                Edge toAdd = new Edge(this,succ);
                e[i] = toAdd;
            }
            if (cacheEdges) this.edges = e;
            return e;
        }
    }

    public boolean equal(PancakeNode n){
        if(n.id.length != this.id.length)
            return false;
        for(int i = 0; i < id.length; i++){
            if (n.id[i] != this.id[i]){
                return false;
            }
        }
        return true;
    }

    public boolean equal(Node n){
        //System.out.println("pancake -> node goal");
        if (n instanceof PancakeNode){
            return this.equal((PancakeNode)n);
        }else{
            return false;
        }
    }

    public String toString(){
        String ret = "[";
        for(int i = 0; i < this.id.length; i++)
            ret += " " + this.id[i];
        ret += "]";
        return ret;
    }
}

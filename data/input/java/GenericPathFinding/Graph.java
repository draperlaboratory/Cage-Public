public class Graph{

    protected Node[] nodes;
    protected Edge[] edges;

    public Graph(Node[] n, Edge[] e){
        this.nodes = n;
        this.edges = e;
    }

    public Node[] getNodes(){
        return this.nodes;
    }

    public Edge[] getEdges(){
        return this.edges;
    }
}

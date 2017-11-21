public class Node{
    protected int id;
    protected Edge[] edges;

    public Node(){
    }

    public Node(int i){
        this.id = i;
        this.edges = new Edge[0];
    }

    public void setEdges(Edge[] e){
        this.edges = e;
    }

    public void addEdge(Edge e){
        Edge[] newEar = new Edge[edges.length + 1];
        for(int i = 0; i < edges.length; i++){
            newEar[i] = this.edges[i];
        }
        newEar[this.edges.length] = e;
        this.edges = newEar;
    }

    public boolean equal(Node n){
        return this.id == n.id;
    }

    public Node[] adjacent(){
        Node[] toRet = new Node[this.edges.length];
        for(int i = 0; i < edges.length; i++){
            toRet[i] = this.edges[i].getDestination();
        }
        return toRet;
    }

    public Edge[] getEdges(){
        return this.edges;
    }

    public Edge findTraversed(Node n){
        for(int i = 0; i < edges.length; i++){
            if(edges[i].getDestination().equal(n)){
                return edges[i];
            }
        }
        return null;
    }

    public static Edge[] findEdges(Node[] nodes){
        Edge[] toRet = new Edge[nodes.length - 1];
        for(int i = 0; i < toRet.length; i++){
            Edge thisEdge = nodes[i].findTraversed(nodes[i+1]);
            if(thisEdge == null){
                return null;
            }
            toRet[i] = thisEdge;
        }
        return toRet;
    }

    public String toString(){
        return "Node " + this.id;
    }

    public void display(){
        System.out.println(this.toString());
    }
}

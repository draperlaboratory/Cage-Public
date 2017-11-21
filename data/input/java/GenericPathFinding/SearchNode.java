public class SearchNode extends Node{

    protected SearchNode parent;

    public SearchNode(){
        this.parent = null;
    }

    public SearchNode(int i){
        super(i);
        this.parent = null;
    }

    public static SearchNode ofNode(Node s){
        SearchNode ret = new SearchNode(s.id);
        ret.parent = null;
        ret.edges = s.edges;
        return ret;
    }

    public static SearchNode ofNodeWParent(SearchNode p, Node s){
        SearchNode ret = ofNode(s);
        ret.parent = p;
        return ret;
    }

    public Node[] adjacent(){
        System.out.println("wo");
        SearchNode[] toRet = new SearchNode[this.edges.length];
        for(int i = 0; i < edges.length; i++){
            Node c = this.edges[i].getDestination();
            toRet[i] = SearchNode.ofNode(c);
            toRet[i].parent = this;
        }
        return toRet;
    }

    public boolean isRoot(){
        return this.parent == null;
    }

    public SearchNode getParent(){
        return this.parent;
    }

    public void setParent(SearchNode s){
        this.parent = s;
    }

    public int pathLength(SearchNode s){
        if (s.isRoot()){
            return 0;
        }else{
            return 1 + this.pathLength(s.parent);
        }
    }

    public Node[] solution(){
        SearchNode goal = this;
        Node[] sol = new Node[this.pathLength(goal) + 1];
        for(int i = sol.length - 1; i >= 0; i-- ){
            sol[i] = goal;
            goal = goal.parent;
        }
        return sol;
    }
}

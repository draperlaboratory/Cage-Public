public class PancakeSearchNode extends SearchNode{

    public PancakeSearchNode(){
        this.parent = null;
    }

    public static PancakeSearchNode ofNode(PancakeNode s){
        PancakeSearchNode ret = new PancakeSearchNode();
        ret.parent = null;
        ret.edges = s.edges;
        System.out.println(ret);
        return ret;
    }

    public static SearchNode ofNodeWParent(PancakeSearchNode p, PancakeNode s){
        PancakeSearchNode ret = PancakeSearchNode.ofNode(s);
        ret.parent = p;
        return ret;
    }
}

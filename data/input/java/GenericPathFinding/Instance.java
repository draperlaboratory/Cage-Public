public abstract class Instance{
    protected SearchNode start;

    public abstract boolean goalP(Node n);

    public SearchNode getStart(){
        return this.start;
    }
}

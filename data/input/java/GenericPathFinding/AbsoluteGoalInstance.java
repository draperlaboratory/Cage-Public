public class AbsoluteGoalInstance extends Instance{
    protected Node goal;

    public AbsoluteGoalInstance(Node s, Node g){
        this.start = SearchNode.ofNode(s);
        this.goal = g;
    }

    public AbsoluteGoalInstance(SearchNode s, Node g){
        this.start = s;
        this.goal = g;
    }

    public boolean goalP(Node n){
        return n.equal(goal);
    }

    public String toString(){
        return "Start: " + this.start + "\tGoal: " + this.goal;
    }

    public void display(){
        System.out.println(this.toString());
    }
}

public class PancakeInstance extends Instance{
    protected PancakeNode goal;

    public PancakeInstance(PancakeNode s, PancakeNode g){
        this.start = s;
        this.goal = g;
    }

    public boolean goalP(PancakeNode n){
        return n.equal(goal);
    }

    public boolean goalP(Node n){
        if (n instanceof PancakeNode){
            return this.goalP((PancakeNode)n);
        }else{
            return false;
        }
    }

    public String toString(){
        return "Start: " + this.start + "\tGoal: " + this.goal;
    }

    public void display(){
        System.out.println(this.toString());
    }
}

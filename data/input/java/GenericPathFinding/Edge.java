public class Edge{
    protected int id;
    protected int cost;
    protected Node origin;
    protected Node destination;

    public Edge(Node s, Node e){
        this.origin = s;
        this.destination = e;
        this.id = -1;
        this.cost = 1;
    }

    public Edge(Node s, Node e, int id){
        this.origin = s;
        this.destination = e;
        this.id = id;
        this.cost = 1;
    }

    public Edge(Node s, Node e, int id, int cost){
        this.origin = s;
        this.destination = e;
        this.id = id;
        this.cost = cost;
    }

    public boolean isSelfLoop(){
        return this.origin == this.destination;
    }

    public String toString(){
        return this.origin + " -> " + this.destination
            + " costing " + this.cost;
    }

    public void display(){
        System.out.println(this.toString());
    }

    public Node getOrigin(){
        return this.origin;
    }

    public Node getDestination(){
        return this.destination;
    }

    public int getCost(){
        return this.cost;
    }

    public static int pathCost(Edge[] edges){
        int total = 0;
        for(int i = 0; i < edges.length; i++){
            total = total + edges[i].getCost();
        }
        return total;
    }
}

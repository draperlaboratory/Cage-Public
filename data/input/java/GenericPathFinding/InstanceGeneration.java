public class InstanceGeneration{

    public static Graph line(int length){
        Node[] n = new Node[length];
        Edge[] e = new Edge[length];

        //make the nodes
        for(int i = 0; i < length; i++){
            n[i] = new Node(i);
        }

        // make the edges
        for(int i = 0; i < length-1; i++){
            int j = i+1;
            Edge a = new Edge(n[i], n[j], i);
            e[i] = a;
            n[i].addEdge(a);
        }

        return new Graph(n,e);
    }

    public static Graph biLine(int length){
        Node[] n = new Node[length];
        Edge[] e = new Edge[length*2];

        //make the nodes
        for(int i = 0; i < length; i++){
            n[i] = new Node(i);
        }

        // make the edges
        int edges = 0;
        for(int i = 0; i < length-1; i++){
            int j = i+1;
            Edge a = new Edge(n[i], n[j], edges);
            Edge b = new Edge(n[j], n[i], edges+1);
            e[edges++] = a;
            e[edges++] = b;
            n[i].addEdge(a);
            n[j].addEdge(b);
        }

        return new Graph(n,e);
    }

    public static Graph Grid(int length, int width){
        int numNodes = length * width;
        Node[] n = new Node[numNodes];
        Edge[] e = new Edge[numNodes * 3 / 2  - 2];

        for(int i = 0; i < numNodes; i++){
            n[i] = new Node(i);
        }

        int edges = 0;
        for(int i = 0; i < numNodes - width; i++){
            Edge toAdd = new Edge(n[i], n[i+width]);
            n[i].addEdge(toAdd);
            e[edges++] = toAdd;
        }
        for(int i = 0; i < numNodes - 1; i++){
            if((i+1) % width != 0){
                Edge toAdd = new Edge(n[i], n[i+1]);
                e[edges++] = toAdd;
                n[i].addEdge(toAdd);
            }
        }
        //System.out.println(numNodes + " " + e.length + " " + edges);
        return new Graph(n,e);
    }
}

public class BreadthFirstBiLine{
    public static void main(String[] argv){
        int sz = argv.length;
        if(sz < 20){
            sz = 20;
        }

        Graph toSolve = InstanceGeneration.biLine(sz);
        Node[] nodes = toSolve.getNodes();
        AbsoluteGoalInstance i = new AbsoluteGoalInstance(nodes[0], nodes[sz - 1]);
        SearchNode sol = BreadthFirstSearch.solve(i);
        i.display();
        if(null != sol){
            Node[] states = sol.solution();
            Edge[] edges = Node.findEdges(states);
            System.out.println("States: " + states.length + "\tEdges: " + edges.length);
            for(int j = 0; j < edges.length; j++){
                edges[j].display();
            }
        }
    }
}

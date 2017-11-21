public class BreadthFirstPancake{

    public static void main(String[] argv){
        int sz = argv.length;
        if(sz < 2){
            sz = 2;
        }

        PancakeNode init = PancakeNode.randomInstance(sz);
        PancakeNode goal = PancakeNode.cannonicalGoal(sz);
        PancakeInstance i = new PancakeInstance(init,goal);
        SearchNode sol = BreadthFirstSearch.solve(i);
        i.display();
        if(null != sol){
            Node[] states = sol.solution();
            Edge[] edges = PancakeNode.findEdges(states);
            System.out.println("States: " + states.length + "\tEdges: " + edges.length);
            for(int j = 0; j < edges.length; j++){
                edges[j].display();
            }
        }
    }
}

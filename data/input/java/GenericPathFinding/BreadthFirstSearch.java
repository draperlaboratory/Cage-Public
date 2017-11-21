public class BreadthFirstSearch{

    public static SearchNode solve(Instance inst){
        SearchNode[] currentLayer;
        SearchNode[][] nextLayer;
        int cIndex = 0;
        int nextSize = 0;
        // set up instance root
        currentLayer = new SearchNode[1];
        currentLayer[0] = inst.getStart();
        // start search loop
        while(currentLayer.length > 0){
            //System.out.println("BFS Open: " + currentLayer.length);
            cIndex = 0;
            nextSize = 0;
            nextLayer = new SearchNode[currentLayer.length][];
            for(int i = 0; i < currentLayer.length; i++){
                SearchNode p = currentLayer[i];
                if(p != null){
                    if(inst.goalP(p)){
                        //System.out.println(p + " is a goal!");
                        // this is the solution
                        return p;
                    }else{
                        //System.out.println("expanding " + p);
                        SearchNode[] toAdd = (SearchNode[])p.adjacent();
                        //System.out.println("adding " + toAdd.length);
                        nextLayer[i] = toAdd;
                        if(toAdd != null) nextSize += toAdd.length;
                    }
                }else{
                    //System.out.println("index " + i + " was null.");
                }
            }

            //all nodes in layer expanded, now move next to current;
            currentLayer = new SearchNode[nextSize];
            for(int i = 0; i < nextLayer.length; i++){
                SearchNode[] toAdd = nextLayer[i];
                if(toAdd != null){
                    for(int j = 0; j < toAdd.length; j++){
                        currentLayer[cIndex++] = toAdd[j];
                    }
                }
            }
        }
        return null;
    }
}

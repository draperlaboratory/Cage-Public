public class DepthFirst{

    private static boolean apply(Move m, Board b){
        return b.update(m.x,m.y,m.value);
    }

    private static Location firstEmpty(Board b){
        for(int x = 0; x < Board.SIZE; x++){
            for(int y = 0; y < Board.SIZE; y++){
                if (b.isEmpty(x,y))
                    return new Location(x,y);
            }
        }
        return null;
    }

    private static Board solveInternal(Board state){
        if (null == state){
            return null; // no solution
        }
        else if (Board.solved(state)){
            return state;
        }else{
            Location next = firstEmpty(state);
            if(null == next){
                return null;
            }else{
                for(int i = 0; i < Board.SIZE; i++){
                    Move thisMove = new Move(next.x, next.y, i);
                    Board statePrime = new Board(state);
                    if(apply(thisMove,statePrime)){
                        Board subtreeSol = solveInternal(statePrime);
                        if(null != subtreeSol && Board.solved(subtreeSol)){
                            return subtreeSol;
                        }
                    } // specific subtree contains no solutions, move on.
                } //entire subtree contains no solution
            }
            return null;
        }
    }

    public static Board solve(Board init){
        Board toReturn = new Board(init);
        return solveInternal(toReturn);
    }

    public static void main(String[] argv){
        System.out.println("Generating Board");
        Board b = new Board(argv[0].length());
        System.out.println("Initial State");
        Board.display(b);
        Board solution = DepthFirst.solve(b);
        if(null != solution)
            Board.display(solution);
        else
            System.out.println("Board was infeasible");
    }
}

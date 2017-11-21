import java.util.Random;

public class Board {

    private static final int EMPTY = -1;
    public static final int SIZE = 9;
    private static final char BLANK = ' ';
    private int[][] rows;
    private int[][] cols;
    private int[][] blocks;

    public Board(){
        this.rows = new int[SIZE][SIZE];
        this.cols = new int [SIZE][SIZE];
        this.blocks = new int[SIZE][SIZE];
        for(int x = 0; x < SIZE; x++){
            for(int y = 0; y < SIZE; y++){
                this.rows[x][y] = EMPTY;
                this.cols[x][y] = EMPTY;
                this.blocks[x][y] = EMPTY;
            }
        }
    }

    // copy constructor
    public Board(Board b){
        this.rows = new int[SIZE][SIZE];
        this.cols = new int [SIZE][SIZE];
        this.blocks = new int[SIZE][SIZE];
        for(int x = 0; x < SIZE; x++){
            for(int y = 0; y < SIZE; y++){
                this.rows[x][y] = b.rows[x][y];
                this.cols[x][y] = b.cols[x][y];
                this.blocks[x][y] = b.blocks[x][y];
            }
        }
    }

    //random board from a seed -- obviously a stub
    public Board(int seed){
        this.rows = new int[SIZE][SIZE];
        this.cols = new int [SIZE][SIZE];
        this.blocks = new int[SIZE][SIZE];
        for(int x = 0; x < SIZE; x++){
            for(int y = 0; y < SIZE; y++){
                this.rows[x][y] = EMPTY;
                this.cols[x][y] = EMPTY;
                this.blocks[x][y] = EMPTY;
            }
        }

        Random rng = new Random(seed);
        int toAdd = rng.nextInt(81);
        for(int i = 0; i < toAdd; i++){
            int x = rng.nextInt(SIZE);
            int y = rng.nextInt(SIZE);
            int c = rng.nextInt(SIZE);
            this.update(x,y,c);
        }
    }

    // does the int array contain the value?
    private boolean contains(int[] cells, int value){
        for(int i = 0; i < SIZE; i++){
            if (cells[i] == value)
                return true;
        }
        return false;
    }

    // compute the offst into the block array from an x/y
    public static int blockInd(int x, int y){
        int blockX = x / 3;
        int blockY = y / 3;
        return blockX + 3 * blockY;
    }

    // can this value be added to these cells
    public boolean feasible(int[] cells, int value){
        return !this.contains(cells,value);
    }

    // is a move feasible
    public boolean feasible(int x, int y, int cell){
        int blockInd = Board.blockInd(x,y);
        return
            this.rows[y][x] == EMPTY &&
            this.feasible(this.rows[y], cell) &&
            this.cols[x][y] == EMPTY &&
            this.feasible(this.cols[x], cell) &&
            this.blocks[blockInd][cell] == EMPTY &&
            this.feasible(this.blocks[Board.blockInd(x,y)], cell);
    }

    // display a board
    public static void display(Board b){
        for(int y = 0; y < b.SIZE; y++){
            for(int x = 0; x < b.SIZE; x++){
                int el = b.rows[y][x];
                if (el == EMPTY){
                    System.out.print(BLANK);
                }else{
                    System.out.print(el+1);
                }
                System.out.print(BLANK);
            }
            System.out.println();
        }
    }

    // is the board completed
    public static boolean solved(Board b){
        for(int x = 0; x < SIZE; x++){
            for(int y = 0; y < SIZE; y++){
                if(b.rows[x][y] == EMPTY ||
                   b.cols[x][y] == EMPTY ||
                   b.blocks[x][y] == EMPTY){
                    return false;
                }
            }
        }
        return true;
    }

    // is the board consistent
    public static boolean consistent(Board b){
        for(int x = 0; x < SIZE; x++){
            for(int y = 0; y < SIZE; y++){
                if(b.rows[x][y] ==  b.cols[x][y]
                   && b.rows[x][y] == b.blocks[x][y]){
                    return false;
                }
            }
        }
        return true;
    }

    // try and put cell in board[x][y], but only if that
    // move is feasible.  Return success of update.
    public boolean update(int x, int y, int cell){
        if(this.feasible(x,y,cell)){
            this.rows[y][x] = cell;
            this.cols[x][y] = cell;
            this.blocks[Board.blockInd(x,y)][cell] = cell;
            return true;
        }else{
            return false;
        }
    }

    public boolean isEmpty(int x, int y){
        return
            this.rows[y][x] == EMPTY &&
            this.cols[x][y] == EMPTY;
    }

    public static void main(String[] argv){
        Board b = new Board();
        Board.display(b);
        System.out.println(b.feasible(0,5,7));
    }
}

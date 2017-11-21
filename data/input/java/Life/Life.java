/* barebones implementation of conway's game of life */

import java.util.Random;
import java.lang.Math;
import java.lang.Integer;

public class Life{

    private static final char LIVING = '#';
    private static final char BLANK = ' ';
    private boolean[][] board;

    public Life(int x, int y){
        this.board = new boolean[y][x];
    }

    public Life(){}

    private void display(){
        int height = board.length;
        int width = board[0].length;
        for(int y = 0; y < height; y++){
            for(int x = 0; x < width; x++){
                if (this.board[y][x]){
                    System.out.print(Life.LIVING);
                }else{
                    System.out.print(Life.BLANK);
                }
            }
            System.out.println(); // newline at end of row
        }
    }

    private boolean nextState(int x, int y, int maxX, int maxY){
        int i = Math.max(0,x-1);
        int j = Math.max(0,y-1);
        maxY = Math.min(y+1, maxY);
        maxX = Math.min(x+1, maxX);
        int surrounding = 0;
        for(; j <= maxY; j++){
            for(; i <= maxX; i++){
                // cell is alive and is not myself
                if(this.board[j][i] && !(j == y && i == x)){
                    surrounding += 1;
                }
            }
        }
        if(surrounding < 2){
            return false; // underpopulation
        }else if (surrounding == 2){
            return this.board[y][x]; //stasis
        }else if (surrounding == 3){
            return true; // breeding
        }else{
            return false; // overcrowding
        }
    }

    private void stepBoard(){
        int maxY = this.board.length;
        int maxX = this.board[0].length;
        boolean[][] next = new boolean[maxY][maxX];
        maxY --;
        maxX --;
        for(int x = 0; x < maxX; x++){
            for(int y = 0; y < maxY; y++){
                next[y][x] = this.nextState(x,y,maxX,maxY);
            }
        }
        // all updates done
        this.board = next;
    }

    private void randomInit(int width, int height){
        this.board = new boolean[height][width];
        Random r = new Random();
        for(int x = 0; x < width; x++){
            for(int y = 0; y < height; y++){
                this.board[y][x] = r.nextBoolean();
            }
        }
    }


    public static void main(String[] argv){
        if(argv.length != 3){
            System.err.println("Expected width, height, steps as arguments");
            System.exit(-1);
        }
        int width = Integer.parseInt(argv[0]);
        int height = Integer.parseInt(argv[1]);
        int steps = Integer.parseInt(argv[2]);

        Life l = new Life();
        l.randomInit(width,height);

        for(int i = 0; i < steps; i++){
            l.display();
            l.stepBoard();
            System.out.println();
        }
        l.display();
    }
}

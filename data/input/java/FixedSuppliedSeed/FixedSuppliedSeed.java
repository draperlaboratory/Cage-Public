import java.util.Random;

public class FixedSuppliedSeed{
    protected static int SECRET = 8675309;
    private Random r;

    public FixedSuppliedSeed(){
        r = new Random(SECRET);
    }

    public FixedSuppliedSeed(int s){
        r = new Random(s);
    }

    public static void display(boolean b){
        if(b)
            System.out.println("Heads");
        else
            System.out.println("Tails");
    }

    public void nFlips(int n){
        for(int i = 0; i < n; i++){
            boolean b = this.flip();
            FixedSuppliedSeed.display(b);
        }
    }

    public boolean flip(){
        return this.r.nextBoolean();
    }

    public static void main(String[] argv){
        FixedSuppliedSeed me;
        int flips = 0;
        int seed;
        if(argv.length >= 1){
            flips = Integer.parseInt(argv[0]);
        }
        if(argv.length >= 2){
            seed = Integer.parseInt(argv[1]);
            me = new FixedSuppliedSeed(seed);
        }else{
            me = new FixedSuppliedSeed();
        }
        me.nFlips(flips);
    }
}

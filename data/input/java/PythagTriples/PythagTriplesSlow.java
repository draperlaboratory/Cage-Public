import java.lang.Math;

public class PythagTriplesSlow{

    private static final int MAGIC = 1000;


    // if we need a sufficiently large value of ints, the performance
    // proflie changes from constant to linear, revealing a feature of
    // MAGIC, namely, it's value.
    public static int[] oddInts(int length){
        int[] ret = new int[length];
        int start = 1;
        for(int i = 0; i < length; i++){
            ret[i] = start;
            start += 2;
        }
        return ret;
    }

    public static int nthSquare(int[] ar, int n){
        int ret = -1;
        double sqr;
        for(int i = 0; i < ar.length; i++){
            sqr = Math.sqrt(ar[i]);
            if ((int)sqr == sqr && sqr * sqr == ar[i]){
                // System.out.println("sqr: " + sqr + " ar[i]: " + ar[i]);
                n --;
                if(n == 0){
                    ret = i;
                }
            }
        }
        return ret;
    }

    private static Triple fibInternal(int nth, int neededOdds){
        int[] odds = PythagTriples.oddInts(neededOdds);
        int asi = nthSquare(odds, nth);
        int a = 0;
        int b = 0;
        int c = 0;
        //System.out.println("nth: " + nth + " odds[" + asi + "] " + odds[asi]);
        if(asi < 0){
            // our guess on the array size wasn't big enough
            // double it and go again
            return(fibInternal(nth, neededOdds * 2));
        }else{
            a = (int) Math.sqrt(odds[asi]);
            for(int i = 0; i < asi; i++){
                b += odds[i];
            }
            c = b + odds[asi];
            return new Triple(a,(int)Math.sqrt(b),(int)Math.sqrt(c));
        }
    }

    public static Triple fibMethod(int nth){
        return fibInternal(nth, MAGIC);
    }

    public static void main(String[] argv){
        int nth = argv.length + 1;
        Triple ans = PythagTriplesSlow.fibMethod(nth);
        ans.display();
    }
}

// Find the largest element of a randomly populated list

import java.util.Random;

public class Largest {
    public static final int MAXVAL = Integer.MAX_VALUE;

    //O(n)
    public static int[] popInts(int count){
        int[] toRet = new int[count];
        Random myRand = new Random();
        for(int i = 0; i < count; i++){
            toRet[i] = myRand.nextInt(MAXVAL);
        }
        return toRet;
    }

    //O(n)
    public static int findLargest(int[] toSearch){
        int toRet = Integer.MIN_VALUE;
        for(int i = 0; i < toSearch.length; i++){
            if (toRet < toSearch[i]){
                toRet = toSearch[i];
            }
        }
        return toRet;
    }

    public static void main(String[] argv){
        if(argv.length == 0){
            System.out.println("O(n)");
        }else{
            int toGen = Integer.parseInt(argv[0]);
            int[] ia = Largest.popInts(toGen);
            int ret = Largest.findLargest(ia);
            System.out.println(ret);
        }
    }
}

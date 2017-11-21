// Print out the number that is equal to the number of ones encountered
// while counting up to it.

public class CountOnes{

    //O(log_10(n))
    public static int onesIn(int num){
        String s = "" + num;
        char[] string = s.toCharArray();
        char one = '1';
        int ret = 0;

        // O(n) in string length
        for(int i = 0; i < s.length(); i++){
            if(one == string[i]) ret++;
        }
        return ret;
    }

    //O(n*log_10(n))
    public static int findConvergance(){
        int seenOnes = 1;
        //O(n), assuming there is such a number (there is)
        for(int current = 2; true; current++){
            //O(log_10(n))
            seenOnes += onesIn(current);
            if (seenOnes == current){
                return current;
            }
        }
    }

    public static void main(String[] argv){
        if(argv.length == 0){
            System.out.println("O(n*log(n))");
        }else{
            System.out.println(CountOnes.findConvergance());
        }
    }
}

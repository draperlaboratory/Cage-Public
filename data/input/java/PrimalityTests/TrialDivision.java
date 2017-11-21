import java.lang.Math;

public class TrialDivision{
    public static int remainder(int i, int j){
        if(i > j){
            return TrialDivision.remainder(i - j, j);
        }else{
            return i;
        }
    }


    public static boolean trialDivision(int n){

        double max = Math.sqrt(n);
        int r = 0;
        if(n == 0 || n == 1) return true;
        if(max == (int) max) return false;

        for(int i = 2; i < max; i++){

            r = TrialDivision.remainder(n,i);
            if (0 == r){
                return false;
            }
        }

        return true;
    }

    public static void main(String[] argv){
        TrialDivision.trialDivision(argv.length);
    }

}

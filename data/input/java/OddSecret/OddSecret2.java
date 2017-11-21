public class OddSecret2 extends OddSecret{
    public static void dispatch(int m){
        int sum = m + SECRET;
        if(sum % 2 == 0){
            OddSecret.countDown(m);
        }else{
            OddSecret.showSum(m);
        }
    }

    public static void main(String[] argv){
        int me = argv.length;
        OddSecret.dispatch(me);
    }
}

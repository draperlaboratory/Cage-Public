public class StaticEvenSecret2 extends StaticEvenSecret{
    public static void dispatch(int m){
        int sum = m + SECRET;
        if(sum % 2 == 0){
            StaticEvenSecret.countDown(m);
        }else{
            StaticEvenSecret.showSum(m);
        }
    }

    public static void main(String[] argv){
        int me = argv.length;
        StaticEvenSecret.dispatch(me);
    }
}

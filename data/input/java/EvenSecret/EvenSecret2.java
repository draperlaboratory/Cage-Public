public class EvenSecret2 extends EvenSecret{

    public void dispatch(int m){
        int sum = m + SECRET;
        if(sum % 2 == 0){
            this.countDown(m);
        }else{
            this.showSum(m);
        }
    }

    public static void main(String[] argv){
        int me = argv.length;
        EvenSecret2 es2 = new EvenSecret2();
        es2.dispatch(me);
    }
}

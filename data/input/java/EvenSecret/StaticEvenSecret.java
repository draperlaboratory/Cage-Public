public class StaticEvenSecret{
    protected static int SECRET = 18;

    public static void display(int i){
        System.out.println(i);
    }

    public static void countDown(int m){
        for(int i = 0; i < m; i++){
            StaticEvenSecret.display(i);
        }
    }

    public static void showSum(int m){
        StaticEvenSecret.display(m);
    }

    public static void dispatch(int m){
        int sum = m + SECRET;
        if(sum % 2 == 0){
            StaticEvenSecret.countDown(sum);
        }else{
            StaticEvenSecret.showSum(sum);
        }
    }

    public static void dispatch2(int m){
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

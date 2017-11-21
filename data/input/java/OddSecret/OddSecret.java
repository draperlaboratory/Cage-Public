public class OddSecret{
    protected static int SECRET = 17;

    public static void display(int i){
        System.out.println(i);
    }

    public static void countDown(int m){
        for(int i = 0; i < m; i++){
            OddSecret.display(i);
        }
    }

    public static void showSum(int m){
        OddSecret.display(m);
    }

    public static void dispatch(int m){
        int sum = m + SECRET;
        if(sum % 2 == 0){
            OddSecret.countDown(sum);
        }else{
            OddSecret.showSum(sum);
        }
    }

    public static void dispatch2(int m){
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

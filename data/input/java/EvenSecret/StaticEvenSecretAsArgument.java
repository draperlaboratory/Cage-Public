public class StaticEvenSecretAsArgument{

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

    public static void dispatch(int m, int SECRET){
        int sum = m + SECRET;
        if(sum % 2 == 0){
            StaticEvenSecret.countDown(sum);
        }else{
            StaticEvenSecret.showSum(sum);
        }
    }

    public static void main(String[] argv){
        int me = argv.length;
        int SECRET = 0;
        for(int i = 0; i < argv.length; i++){
            SECRET += argv[0].length();
        }

        StaticEvenSecretAsArgument.dispatch(me,SECRET);
    }
}

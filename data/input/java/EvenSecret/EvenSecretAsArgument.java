public class EvenSecretAsArgument{

    public void display(int i){
        System.out.println(i);
    }

    public void countDown(int m){
        for(int i = 0; i < m; i++){
            this.display(i);
        }
    }

    public void showSum(int m){
        this.display(m);
    }

    public void dispatch(int m, int SECRET){
        int sum = m + SECRET;
        if(sum % 2 == 0){
            this.countDown(sum);
        }else{
            this.showSum(sum);
        }
    }

    public static void main(String[] argv){
        int me = argv.length;
        int SECRET = 0;
        for(int i = 0; i < argv.length; i++){
            SECRET += argv[0].length();
        }

        EvenSecretAsArgument esa = new EvenSecretAsArgument();

        esa.dispatch(me,SECRET);
    }
}

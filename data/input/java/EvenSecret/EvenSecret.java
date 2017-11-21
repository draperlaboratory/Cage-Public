public class EvenSecret{
    protected int SECRET = 18;

    //Hack to make initializer tainted
    static EvenSecret init(){
        return new EvenSecret();
    }
    
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

    public void dispatch(int m){
        int sum = m + SECRET;
        if(sum % 2 == 0){
            this.countDown(sum);
        }else{
            this.showSum(sum);
        }
    }

    public void dispatch2(int m){
        int sum = m + SECRET;
        if(sum % 2 == 0){
            this.countDown(m);
        }else{
            this.showSum(m);
        }
    }

    public static void main(String[] argv){
        int me = argv.length;
        EvenSecret es = EvenSecret.init();
        es.dispatch(me);
    }
}

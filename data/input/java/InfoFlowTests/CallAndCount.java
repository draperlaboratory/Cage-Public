public class CallAndCount{
    private int value = 0;

    public CallAndCount(int i){
        this.value = i;
    }

    public void count(){
        for(int i = 0; i < this.value; i++){
        }
    }

    public static void main(String[] argv){
        int i = argv.length;
        CallAndCount cac = new CallAndCount(i);
        cac.count();
    }
}

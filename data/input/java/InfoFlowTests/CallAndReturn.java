public class CallAndReturn{
    private int value = 0;

    public CallAndReturn(int i){
        this.value = i;
    }

    public int getValue(){
        return this.value;
    }

    public void setValue(int i){
        this.value = i;
    }

    public static void main(String[] argv){
        CallAndReturn car = new CallAndReturn(argv.length);
        int len = car.getValue();
    }
}

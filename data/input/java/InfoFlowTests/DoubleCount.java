public class DoubleCount{
    public static void count(int c, int d){
        for(int i = 0; i < c * d; i++){}
    }

    public static void main(String[] argv){
        int i = argv.length;
        DoubleCount.count(i,i);
    }
}

public class LogSecret{
    private static int SECRET = 3;

    public static void display(int i){
        System.out.println(i);
    }

    public static void countDown(int m){
        for(int i = m; i > 0; i /= SECRET){
            LogSecret.display(i);
        }
    }

    public static void main(String[] argv){
        int start = argv.length;
        LogSecret.countDown(start);
    }
}

import java.util.Random;
public class SecretRand{
    private static int SECRET = 17;
    private static int MAX = 100;

    public static void display(int i){
        System.out.println(i);
    }

    public static void countDown(int m){
        for(int i = 0; i < m; i++){
            display(i);
        }
    }

    public static void main(String[] argv){
        int seed = argv.length % SECRET;
        Random r = new Random(seed);
        int cd = r.nextInt(MAX);
        SecretRand.countDown(cd);
    }
}

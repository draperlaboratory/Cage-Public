public class Fib {
    //EXP
    public static void call(int i) {
        int a = 0, b = 1, c;
        for(int x = 0; x < i; x = x+1) {
            c = b;
            b = b+a;
            a = c;
        }
        while (b > 0) b--;
    }

    public static void main(String[] argv){
        Fib.call(argv.length);
    }
}

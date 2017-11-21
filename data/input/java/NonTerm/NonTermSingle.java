public class NonTermSingle{
    public static void a(int i){
        a(i);
    }

    public static void main(String [] argv){
        a(argv[0].length());
    }
}

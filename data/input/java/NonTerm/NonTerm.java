public class NonTerm{
    public static void a(int i){
        b(i);
    }

    public static void b(int i){
        a(i);
    }

    public static void main(String [] argv){
        a(argv[0].length());
    }
}

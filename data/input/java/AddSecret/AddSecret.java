public class AddSecret{
    private static int SECRET = 42;

    public static void display(int i){
        System.out.println(i);
    }

    public static void countDown(int m){
        for(int i = 0; i < m; i++){
            AddSecret.display(i);
        }
    }

    public static void main(String[] argv){
        int start = argv.length + SECRET;
        AddSecret.countDown(start);
    }
}

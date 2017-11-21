public class TestStaticPrint{

    public static void display(int i){
        System.out.println(i);
    }

    public static void main(String[] argv){
        int i = argv.length;
        TestStaticPrint.display(i);
    }
}

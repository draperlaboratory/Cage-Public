public class TestPrint{
    private int i;

    public TestPrint(int i){
        this.i = i;
    }

    public void display(){
        //System.out.println(this.i);
    }

    public static void main(String[] argv){
        int i = argv.length;
        TestPrint tp = new TestPrint(i);
        tp.display();
    }
}

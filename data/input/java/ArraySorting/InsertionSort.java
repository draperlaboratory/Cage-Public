public class InsertionSort{
    public static void main(String[] argv){
        IntArray iar = new IntArray(argv.length);
        iar.display();
        iar.insertionSort();
        iar.display();
    }
}

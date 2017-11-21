public class SelectionSort{
    public static void main(String[] argv){
        IntArray iar = new IntArray(argv.length);
        iar.display();
        iar.selectionSort();
        iar.display();
    }
}

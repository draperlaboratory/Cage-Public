public class BucketSort{
    public static void main(String[] argv){
        IntArray iar = new IntArray(argv.length);
        iar.display();
        iar.bucketSort();
        iar.display();
    }
}

public class RodCuttingDyn{
    public static void main(String[] argv){
        int arr[] = {1, 5, 8, 9, 10, 17, 17, 20};
        int size = argv.length;
        RodCutting rc = new RodCutting(arr);
        int solution2 = rc.cutRodDyn(size);
    }
}

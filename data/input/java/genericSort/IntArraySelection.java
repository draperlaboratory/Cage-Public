public class IntArraySelection{

    public static void main(String[] argv){
        int sz = argv.length;
        if(20 > argv.length){
            sz = 20;
        }
        IntArray iar = IntArray.revOrder(sz);
        iar.display();
        SelectionSort.sort(iar);
        iar.display();
    }

}

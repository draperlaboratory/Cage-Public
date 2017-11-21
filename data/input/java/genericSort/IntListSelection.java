public class IntListSelection{

    public static void main(String[] argv){
        int sz = argv.length;
        if(20 > argv.length){
            sz = 20;
        }
        IntList iar = IntList.revOrder(sz);
        iar.display();
        SelectionSort.sort(iar);
        iar = (IntList)iar.head(iar);
        iar.display();
    }

}

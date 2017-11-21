public class IntArrayBubble{

    public static void main(String[] argv){
        int sz = argv.length;
        if(20 > argv.length){
            sz = 20;
        }
        IntArray iar = IntArray.revOrder(sz);
        iar.display();
        BubbleSort.sort(iar);
        iar.display();
    }

}

public class CharArrayBubble{

    public static void main(String[] argv){
        int sz = argv.length;
        if(20 > argv.length){
            sz = 20;
        }
        CharArray iar = CharArray.revOrder(sz);
        iar.display();
        BubbleSort.sort(iar);
        iar.display();
    }

}

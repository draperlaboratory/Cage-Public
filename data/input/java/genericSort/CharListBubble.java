public class CharListBubble{

    public static void main(String[] argv){
        int sz = argv.length;
        if(20 > argv.length){
            sz = 20;
        }
        CharList iar = CharList.revOrder(sz);
        iar.display();
        BubbleSort.sort(iar);
        iar = (CharList)iar.head(iar);
        iar.display();
    }

}

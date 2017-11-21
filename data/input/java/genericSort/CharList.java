public class CharList extends CompList{
    public CharList(int i){
        super(new MyChar(i));
    }

   public static CharList revOrder(int size){
        CharList[] lists = new CharList[size];
        for(int i = 0; i < size; i++){
            lists[i] = new CharList(size - i);
        }

        for(int i = 0; i < size; i++){
            int p = i - 1;
            int n = i + 1;
            if(i > 0){
                lists[i].setPrev(lists[p]);
            }
            if(n < size){
                lists[i].setNext(lists[n]);
            }
        }
        return lists[0];
    }

}

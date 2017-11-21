public class IntList extends CompList{

    public IntList(int i){
        super(new MyInt(i));
    }

   public static IntList revOrder(int size){
        IntList[] lists = new IntList[size];
        for(int i = 0; i < size; i++){
            lists[i] = new IntList(size - i);
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

    public static void main(String [] argv){
        IntList il = revOrder(2);
        IntList two = il;
        IntList one = (IntList)il.next;
        two.display();
        one.display();
        one.next = two;
        one.prev = two.prev;
        two.next = null;
        two.prev = one;
        one.display();
        two.display();

        il = revOrder(2);
        il.display();
        il.swap(0,1);
        il.display();
    }
}

public class LinkedList extends List {
    private int me;
    private LinkedList next;

    //make a bad list using the default constructor
    // constant
    public LinkedList(){
        next = this;
    }

    // constant
    public LinkedList(int el){
        this.me = el;
        this.next = null;
    }

    // constant
    public void setMe(int el){
        this.me = el;
    }

    // linear scan to append to the end of a linked list.
    // O(n), where n is the size of the list
    public LinkedList append(int el){
        if(this.me == el){
            return this;
        }else if(next == null){
            LinkedList toRet = new LinkedList(el);
            this.next = toRet;
            return toRet;
        }else{
            return next.append(el);
        }
    }

    //linear scan to find an elment.  Returns Null on failuer
    // O(n), where n is the size of the list
    public LinkedList find(int el){
        if(this.me == el){
            return this;
        }else if (this.next == null){
            return null;
        }else{
            return next.find(el);
        }
    }
}

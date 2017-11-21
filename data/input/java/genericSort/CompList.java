public class CompList extends Sortable{
    protected Comp element;
    protected CompList prev;
    protected CompList next;

    public CompList(Comp c){
        this.element = c;
        this.next = null;
        this.prev = null;
    }

    public CompList(CompList cl){
        this.element = cl.element;
        if(cl.next != null){
            this.next = new CompList(cl.next);
        }
        if(cl.prev != null)
            this.prev = new CompList(cl.prev);
    }

    public void setNext(CompList n){
        this.next = n;
        if(n != null){
            n.prev = this;
        }
    }

    public void setPrev(CompList p){
        this.prev = p;
        if(p != null){
            p.next = this;
        }
    }

    public void display(){
        System.out.println(this.toString());
    }

    public String toString(){
        CompList cur = this;
        String ret = "(";
        while(cur != null){
            ret += " ";
            ret +=  cur.element;
            cur = cur.next;
        }
        ret += " )";
        return ret;
    }

    public boolean sorted(int i, int j){
        int ret = this.element.comp(this.ith(i), this.ith(j));
        // memoize for efficiency later
        return ret == Comp.SMALLER || ret == Comp.EQUAL;
    }

    public Sortable swap(int i, int j){
        if(i == j) return this;
        if(i > j){
            int temp = i;
            i = j;
            j = temp;
        }

        CompList iL = this.ithListEl(i);
        CompList jL = this.ithListEl(j);
        CompList iN = iL.next;
        CompList iP = iL.prev;
        CompList jN = jL.next;
        CompList jP = jL.prev;

        if(j - i == 1){
            iL.setNext(jN);
            iL.setPrev(jL);
            jL.setNext(iL);
            jL.setPrev(iP);

        }else{
            iL.setNext(jN);
            iL.setPrev(jP);
            jL.setNext(iN);
            jL.setPrev(iP);
        }

        assert(iL.prev == null ||
               iL.prev.next == iL);

        assert(iL.next == null ||
               iL.next.prev == iL);


        assert(jL.prev == null ||
               jL.prev.next == jL);

        assert(jL.next == null ||
               jL.next.prev == jL);

        return this.head(iL);
    }

    public int size(){
        if(this.next == null){
            return 1;
        }else{
            return 1 + next.size();
        }
    }

    public Comp minElement(){
        if(this.next == null){
            return this.element;
        }else{
            Comp pos = next.minElement();
            if(element.comp(pos) == Comp.SMALLER){
                return element;
            }else{
                return pos;
            }
        }
    }

    public int minIndexBeyond(int start){
        int origInd = start;
        CompList l = this;
        // deal with the beyond issue
        while(start > 0 && l != null){
            start --;
            l = l.next;
        }
        if (start != 0){
            return -1;
        }
        Comp pos = l.element;
        int retInd = origInd;
        start = origInd;
        while(l != null){
            Comp cons = l.element;
            if(cons.comp(pos) == Comp.SMALLER){
                pos = cons;
                retInd = start;
            }
            l = l.next;
            start++;
        }
        return retInd;
    }

    public Comp maxElement(){
        if(this.next == null){
            return this.element;
        }else{
            Comp pos = next.minElement();
            if(element.comp(pos) == Comp.LARGER){
                return element;
            }else{
                return pos;
            }
        }
    }

    public CompList ithListEl(int i){
        if(i == 0){
            return this;
        }else{
            return this.next.ithListEl(i - 1);
        }
    }

    public Comp ith(int i){
        if(i == 0){
            return this.element;
        }else{
            return this.next.ith(i - 1);
        }
    }

    public CompList head(CompList el){
        while(el != null && el.prev != null){
            el = el.prev;
        }
        return el;
    }
}

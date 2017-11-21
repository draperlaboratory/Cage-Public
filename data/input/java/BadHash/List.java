public class List{
    private String element;
    private List next;

    public List(String s){
        this.element = s;
        this.next = null;
    }

    public void add(String s){
        List l = this;
        while(l.next != null){
            l = l.next;
        }

        this.next = new List(s);
    }

    public boolean contains(String s){
        if (this.element.equals(s)){
            return true;
        }else if (this.next == null){
                return false;
        } else {
            return this.next.contains(s);
        }
    }

    public List remove(String s){
        if(this.element.equals(s)){
            return this.next;
        }else{
            this.next = next.remove(s);
            return this.next;
        }
    }
}

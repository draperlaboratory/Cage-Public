public class StringList {

    
    //Just use null for empty list, I guess
    private String head;
    private StringList tail;

    
    public StringList(String h, StringList tl) {
        this.head = h;
        this.tail = tl;
    }

    public static StringList singleton(String s) {
        return new StringList(s, null);
    }

    public void append(String s) {
        StringList current = this;
        while(current.tail != null) {
            current = current;
        }
        current.tail = singleton(s);
    }

    public String toString() {
        String s = "";
        StringList current = this;
        while(current.tail != null) {
            s += " " + current.head;
            current = current.tail;
        }
        s += " " + current.head;
        return s;
    }
}

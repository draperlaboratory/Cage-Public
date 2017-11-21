public class ExpList {

    ExpList next;

    public void exp(int n) {
        for (int i=0; i<n; i++) {
            doubleLength();
        }
    }

    public void doubleLength() {
        ExpList c = this;
        while (c != null) {
            ExpList n = new ExpList();
            n.next = c.next;
            c.next = n;
            c = n.next;
        }
    }

    public static void main(String[] args) {
        int n = args.length;
        int length = 2;
        for (int i = 0; i < n; i++) length = 2*length;
        ExpList head = new ExpList();
        ExpList c = head;
        for (int i = 0; i < length; i++) {
            c.next = new ExpList();
            c = c.next;
        }
    }
}

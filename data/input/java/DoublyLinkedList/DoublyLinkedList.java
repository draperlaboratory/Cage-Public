public class DoublyLinkedList {

    private DoublyLinkedList next, prev;

    public void append() {
        DoublyLinkedList current = this;
        while (current.next != null) {
            current = current.next;
        }
        current.next = new DoublyLinkedList();
        current.next.prev = current;
    }

    public static void main(String[] args) {
        int i = args.length;
        DoublyLinkedList l = new DoublyLinkedList();
        for (int j = 0; j < i; j++) l.append();
    }
}

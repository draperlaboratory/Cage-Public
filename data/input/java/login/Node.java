
class Node extends List {
    int head;
    List tail;

    //constant
    public Node(int h, List t){
	head = h;
	tail = t;
    }

    //O(n)
    boolean find(int x) {
	boolean b = tail.find(x);
	if (head == x)
	    {
		return true;
	    }
	else
	    {
		return b;
	    }
    }

}

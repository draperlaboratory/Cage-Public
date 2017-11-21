
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
	if (head == x)
	    {
		return true;
	    }
	else 
	    {
		return tail.find(x);
	    }
    }

}

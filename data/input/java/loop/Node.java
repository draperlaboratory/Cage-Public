class Node extends List {
    int head;
    List tail;

    public Node(int h, List t){
	head = h;
	tail = t;
    }

    //O(n)
    public List append(int x) {
	return tail.append(x);
    }

    //Constant
    public void setTail(List l){
	tail = l;
    }

}

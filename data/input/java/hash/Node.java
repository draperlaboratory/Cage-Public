
class Node extends List {
    int head;
    List tail;

    //constant
    public Node(int h, List t){
	head = h;
	tail = t;
    }

    //O(n), where n is the length of the longest path through the list.
    public List append(int x) {
	return tail.append(x);
    }

}


class Nil extends List {

    //constant
    public List append(int x) {
	return new Node(x, new Nil());
    }
}

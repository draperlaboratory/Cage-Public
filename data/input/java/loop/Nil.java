
class Nil extends List {

    //Constant
    public List append(int x) {
	return new Node(x, new Nil());
    }
}

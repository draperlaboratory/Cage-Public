
class A {
    int pass;
    String name;

    //constant
    public A(int p, String nm){
        this.pass = p;
        this.name = nm;
    }

    //constant
    public A(int p){
        this.pass = p;
        this.name = "" + p;
    }

    //constant
    public Boolean isPass (int q){
	return q == pass;
    }

    //constant
    public String getName(){
	return name;
    }
}


abstract class List {
    abstract String find(int x);
}


class Node extends List {
    A head;
    List tail;

    //constant
    public Node(int h, List t){
	head = new A(h);
	tail = t;
    }

    //O(n)
    String find(int x) {
        A toFind = new A(x);
	if (toFind.isPass(x))
	    {
		return toFind.getName();
	    }
	else
	    {
		return tail.find(x);
	    }
    }

}

class Nil extends List {

    //Constant
    String find(int x) {
	return "";
    }
}


class Account {
    List accounts;

    //O(n)
    int login (String name, int pass) {
	String s = accounts.find(pass);
	if(s == name)
	    {
		return 1;
	    }
	else
	    {
		return 0;
	    }
    }

    public void main(String[] args) {
        if(args.length == 0){
            System.out.println("O(n)");
        }else{
            Account a = new Account();
            a.login(args[0], args[1].length());
        }
    }
}

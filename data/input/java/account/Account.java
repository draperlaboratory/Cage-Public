
class A {
    int pass;
    String name;

    // constant
    public A(int h){
        this.pass = h;
        this.name = "" + h;
    }

    // constant
    public Boolean isPass (int q){
	return q == pass;
    }

    // constant
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

    // constant
    public Node(int h, List t){
	head = new A(h);
	tail = t;
    }

    // O(n), where n is list size \ length of longest path through list.
    String find(int x) {
	if (head.isPass(x))
	    {
		return head.getName();
	    }
	else
	    {
		return tail.find(x);
	    }
    }

}

class Nil extends List {

    // constant
    String find(int x) {
	return "";
    }
}


class Account {
    List accounts;

    // O(n)
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
}

class Login {

    Account a = new Account();

    public void main(String[] args) {
        if(args.length == 0){
            System.out.println("O(n)");
        }else{
            a.login(args[0], args[1].length());
        }
    }
}

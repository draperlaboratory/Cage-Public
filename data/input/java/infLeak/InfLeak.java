
abstract class List {
    abstract boolean find(int x);
}


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


class Nil extends List {

    //constant
    boolean find(int x) {
	return false;
    }
}


class Account {
    List accounts;

    //O(n)
    int login (int pass) {
	accounts.find(pass);
	return 0;
    }
}




class Login {

    Account a = new Account();

    public void main(String[] args) {
        if(args.length == 0){
            System.out.println("O(n)");
        }else{
            a.login(args[0].length());
        }
    }
}



abstract class List {
    public abstract List append(int x);
}

class Nil extends List {

    //constant
    public List append(int x) {
	return new Node(x, new Nil());
    }
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
    public List append(int x) {
	return tail.append(x);
    }

}



class HashTbl {

    private List[] table;

    // constant
    public HashTbl () {

	table = new List[] { new Nil(), new Nil(), new Nil() };
    }

    //O(n)
    public void insert(int x) {
	int hash_x = hashVal(x);
	table[hash_x] = table[hash_x].append(x);
    }

    //constant
    private int hashVal(int y) {
	return y % 3;
    }
}



public class Hash {

    public static void main(String[] args) {
        if(args.length == 0){
            System.out.println("O(n^2)");
        }else{
            HashTbl H = new HashTbl();

            //O(n^2)
            for(int i = 0; i < args[0].length(); i++)
                H.insert(i); //O(n)
        }
    }
}

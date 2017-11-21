public class Node{

    private Node parent;
    private Node leftChild;
    private Node rightChild;
    private int id;

    // various constructors
    public Node(int i){
        this.id = i;
        this.parent = null;
        this.leftChild = null;
        this.rightChild = null;
    }

    public Node(Node p, int me){
        this.id = me;
        this.parent = p;
        this.leftChild = null;
        this.rightChild = null;
    }

    public Node(Node p, Node l, Node r, int me){
        this.id = me;
        this.parent = p;
        this.leftChild = l;
        this.rightChild = r;
    }

    // some accessor functions
    public void setChild(Node c, boolean left){
        if(c != null){
            c.setParent(this);
        }
        if(left){
            this.leftChild = c;
        }else{
            this.rightChild = c;
        }
    }

    public void setParent(Node p){
        this.parent = p;
    }

    public Node getParent(){
        return this.parent;
    }

    public Node getChild(boolean left){
        if(left){
            return this.leftChild;
        }else{
            return this.rightChild;
        }
    }

    // display the node
    public void display(){
        System.out.println("N: " + this.id);
    }

    public static Node makeBalancedTree(int startIndex, int count){
        if(count <= 0){
            return null;
        }

        //        System.out.println("Making " + startIndex);
        Node me = new Node(startIndex++);
        count --;

        int left = count / 2;
        int right = count - left; // deals with uneven numbers.

        //        System.out.println("LST: " + left);
        //        System.out.println("RST: " + right);
        me.setChild(Node.makeBalancedTree(startIndex, left), true);
        me.setChild(Node.makeBalancedTree(startIndex + left, right),
                    false);
        return me;
    }


}

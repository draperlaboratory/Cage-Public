public class Traversals{

    public static void inOrder(Node n){
        if(n != null){
            inOrder(n.getChild(true));
            n.display();
            inOrder(n.getChild(false));
        }
    }

    public static void preOrder(Node n){
        if(n != null){
            n.display();
            preOrder(n.getChild(true));
            preOrder(n.getChild(false));
        }
    }

    public static void postOrder(Node n){
        if(n != null){
            postOrder(n.getChild(true));
            postOrder(n.getChild(false));
            n.display();
        }
    }

    public static void main(String[] argv){
        Node tree = Node.makeBalancedTree(0,32);
        //        System.out.println("In Order");
        inOrder(tree);
        //        System.out.println("Pre Order");
        preOrder(tree);
        //        System.out.println("Post Order");
        postOrder(tree);
    }
}

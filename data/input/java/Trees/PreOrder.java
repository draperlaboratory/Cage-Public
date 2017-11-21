public class PreOrder{

    public static void main(String[] argv){
        Node tree = Node.makeBalancedTree(0,32);
        Traversals.preOrder(tree);
    }
}

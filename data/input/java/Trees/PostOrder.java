public class PostOrder{

    public static void main(String[] argv){
        Node tree = Node.makeBalancedTree(0,32);
        Traversals.postOrder(tree);
    }
}

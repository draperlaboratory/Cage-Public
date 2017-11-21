public class LoopAsRecursion{

    //O(n)
    public void printNums(int i){
        System.out.println(i);
        if (i > 0){
            this.printNums(i-1);
        }else{
            System.out.println("END");
        }
    }

    public static void main(String[] argv){
        if(argv.length == 0){
            System.out.println("O(n)");
        }else{
            LoopAsRecursion me = new LoopAsRecursion();
            me.printNums(argv[0].length());
        }
    }
}

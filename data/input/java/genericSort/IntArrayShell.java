public class IntArrayShell{

    public static void main(String[] argv){
        int sz = argv.length;
        if(20 > argv.length){
            sz = 20;
        }
        IntArray iar = IntArray.revOrder(sz);
        iar.display();
        ShellSort.sort(iar);
        iar.display();
    }

}

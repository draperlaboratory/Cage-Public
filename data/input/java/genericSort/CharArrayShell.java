public class CharArrayShell{

    public static void main(String[] argv){
        int sz = argv.length;
        if(20 > argv.length){
            sz = 20;
        }
        CharArray iar = CharArray.revOrder(sz);
        iar.display();
        ShellSort.sort(iar);
        iar.display();
    }

}

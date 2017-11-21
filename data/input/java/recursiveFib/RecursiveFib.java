
public class RecursiveFib {

    //O(2^n)
    public static int doIt(int i){
        if (i == 0) return 1;
        else if (i == 1) return 1;
        else return (RecursiveFib.doIt(i - 1) + RecursiveFib.doIt(i - 2));
    }

    public static void main(String[] argv){
        if(argv.length == 0){
            System.out.println("O(2^n)");
        }else{
            int nth = Integer.parseInt(argv[0]);
            System.out.println(doIt(nth));
        }
    }
}

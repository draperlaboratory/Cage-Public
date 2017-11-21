
public class Fib {

    //O(n)
    public static int call(int i) {
	int a = 0, b = 1, c;
	for(int x = 0; x < i; x = x+1) {
	    c = b;
	    b = b+a;
	    a = c;
	}
	return b;
    }

    public static void main(String[] args) {
        if(args.length == 0){
            System.out.println("O(n)");
        }else{
            call(args.length);
        }
    }

}


public class Test {


    public static int bar(Faz z) {
	Faz y = new Faz();
	while(z.fizz == 0){
	    y.fizz = 0;
	    System.out.println("Weeee");
	}
	return y.fizz;
    }

    public static int whileIf(Faz z) {
	Faz y = new Faz();
	while(z.fizz == 0){
            if (y.fizz > 0) {
                y.fizz = 0;
            } else {
                y.fizz = 1;
            }
	}
	return y.fizz;
    }

    public static void main(String[] args) {
	Foo f = new Foo();
	Faz z = new Faz();
	f.taints(z);
	whileIf(z);
	System.out.println(args[0]);
    }
}

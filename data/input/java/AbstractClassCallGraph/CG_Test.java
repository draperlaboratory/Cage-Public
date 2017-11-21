import java.util.Random;

public class CG_Test {
	
	public static void main(String[] args) {
		AbstractMethodClass x;
		int result;
		int rand = new Random().nextInt();
		
		if (rand % 10 > 5) {
			x = new ClassA();
		} else {
			x = new ClassB();
		}
		
		result = x.foo();
		
		System.out.println(result);
		
	}
	

}

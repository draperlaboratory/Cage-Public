import java.util.Random;

public class ClassA extends AbstractMethodClass {
	
	ClassA() {}
	
	public int foo() {
		int x;
		int y;
		
		x = new Random().nextInt();
		
		y = x % 10;
		
		return y-x;
	}

}

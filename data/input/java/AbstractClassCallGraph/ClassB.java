import java.util.Random;

public class ClassB extends AbstractMethodClass {
	
	ClassB(){}
	
	@Override
	public int foo() {
		int x = 7;
		int y = 2;
		int rand = new Random().nextInt();
		
		return x+y*rand % 100;
	}

}

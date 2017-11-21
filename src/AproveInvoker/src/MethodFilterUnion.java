import soot.SootMethod;
import soot.jimple.toolkits.annotation.purity.SootMethodFilter;

public class MethodFilterUnion implements SootMethodFilter{
	
	private SootMethodFilter f1;
	private SootMethodFilter f2;
	
	MethodFilterUnion(SootMethodFilter f1, SootMethodFilter f2) {
		this.f1 = f1;
		this.f2 = f2;
	}

	@Override
	public boolean want(SootMethod m) {
		return this.f1.want(m) && this.f2.want(m);
	}

}

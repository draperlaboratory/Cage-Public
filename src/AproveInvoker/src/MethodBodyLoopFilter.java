import java.util.Collection;

import soot.*;
import soot.jimple.toolkits.annotation.logic.*;
import soot.jimple.toolkits.annotation.purity.*;

public class MethodBodyLoopFilter implements SootMethodFilter{

	@Override
	public boolean want(SootMethod m) {

		Body methodBody = m.getActiveBody();
		LoopFinder loopFinder = new LoopFinder();
		loopFinder.transform(methodBody);
		Collection<Loop> loops = loopFinder.loops();
		return !loops.isEmpty();
	}

}

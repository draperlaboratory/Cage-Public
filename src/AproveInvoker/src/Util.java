import java.io.*;
import java.util.*;

import soot.*;

public class Util {

	public static String getDescriptor(SootMethod m) {
		String nearlySig = m.getBytecodeSignature();
		// now we have to trim the leading and closing < > from the string
		// why soot adds these, god only knows
		nearlySig = nearlySig.substring(1, nearlySig.length() - 1);
		// now we need to split on the colon to get classsig : methodSig
		String[] sigSegs = nearlySig.split(":");
		nearlySig = sigSegs[0] + "." + sigSegs[1].substring(1);
		return nearlySig;
	}

	public static List<String> readAndCloseBuffer(BufferedReader buf) throws IOException {
		List<String> res = new ArrayList<>();
		for (String line = buf.readLine(); line != null; line = buf.readLine()) {
			res.add(line);
		}
		buf.close();
		return res;
	}

}

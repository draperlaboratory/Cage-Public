package com.draper.cage.soot;

import java.util.Collection;

import soot.Body;
import soot.jimple.toolkits.annotation.logic.Loop;
import soot.toolkits.graph.LoopNestTree;

public class Util
{
	public static <T> boolean lt(Comparable<? super T> c1, T c2) {
		return c1.compareTo(c2) < 0;
	}

	public static <T> boolean lteq(Comparable<? super T> c1, T c2) {
		return c1.compareTo(c2) <= 0;
	}

	public static <T> boolean eq(Comparable<? super T> c1, T c2) {
		return c1.compareTo(c2) == 0;
	}

	public static <T> boolean gt(Comparable<? super T> c1, T c2) {
		return c1.compareTo(c2) > 0;
	}

	public static <T> boolean gteq(Comparable<? super T> c1, T c2) {
		return c1.compareTo(c2) >= 0;
	}

	public static Collection<Loop> getLoops(Body body) {
		return new LoopNestTree(body);
	}
}

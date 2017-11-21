import java.util.function.*;
public class Lambdas {

    public Lambdas(int i ) {
        while (i > 0) i--;
    }

    private int f(int i) {
        while (i > 0) i--;
        return i;
    }

    private static int fStatic(int i) {
        while (i > 0) i--;
        return i;
    }

    public static interface I {
        int f(int i);
    }

    public static interface IO {
        int f(int i, Object o);
    }

    public static interface LI {
        Lambdas f(int i);
    }

    public static void constructor(int i) {
        gLambda(Lambdas::new, i);
    }

    public static void instanceMethodRef(int i) {
        Lambdas l = new Lambdas(0);
        gSimple(l::f, i);
    }

    public static void staticMethodRef(int i) {
        Lambdas l = new Lambdas(0);
        gSimple(Lambdas::fStatic, i);
    }

    public static void simpleLambdaLoop(int i) {
        gSimple(x -> {while (x > 0) x--; return x;}, i);
    }

    public static void twoArgsLambdaLoop(int i, Object o) {
        gTwoArgs((x,y) -> {while (x > 0) x--; return x;}, i, o);
    }

    public static void lambdaLoopWithClosure(int i) {
        int y = 0;
        gSimple(x -> {while (x > 0) x--; return y;}, i);
    }

    public static void lambdaLoopWithTwoArgsClosure(int i) {
        int y = i;
        Object o = new Object();
        gSimple(x -> {x = y; while (x > 0) x--; return o.hashCode();}, i);
    }

    public static void lambdaLoopWithImportantClosure(int i, int j) {
        gSimple(x -> {x = j; while (x > 0) x--; return x;}, i);
    }

    public static int gSimple(I i, int j) {
        return i.f(j);
    }

    public static Lambdas gLambda(LI i, int j) {
        return i.f(j);
    }

    public static int gTwoArgs(IO i, int j, Object o) {
        return i.f(j, o);
    }

    public static void apiLambdaLoopWithImportantClosure(int i, int j) {
        gApi(x -> {x = j; while (x > 0) x--; return x;}, i);
    }

    public static void apiLambda(int i) {
        gApi(x -> x, i);
    }

    public static int gApi(Function<Integer, Integer> f, int j) {
        return f.apply(j);
    }

}

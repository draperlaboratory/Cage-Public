public abstract class Comp{
    public static final int EQUAL = 0;
    public static final int SMALLER = -1;
    public static final int LARGER = 1;
    public static final int INCOMPARABLE = 17;

    public static int comp(Comp c1, Comp c2){
        return c1.comp(c2);
    }

    public abstract int comp(Comp c1);
}

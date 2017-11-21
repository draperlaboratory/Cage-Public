public abstract class Sortable{

    public abstract boolean sorted(int i, int j);

    public abstract Sortable swap(int i, int j);

    public abstract int size();

    public abstract Comp minElement();

    public abstract int minIndexBeyond(int start);

    public abstract Comp maxElement();

    public abstract Comp ith(int i);

}

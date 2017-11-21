
class Cons<X> extends List<X> {

    X hd;
    List<X> tail;

    public Cons(X x, List<X> xs){
        hd = x;
        tail = xs;
    }

    public boolean find(X x) {
        if (x.equals(hd))
            return true;
        else
            return tail.find(x);

        }

}

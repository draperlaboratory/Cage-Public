
class Cons<X, Y> extends List<X, Y> {

    X hd;
    List<X, Y> tail;

    public Cons(X x, List<X, Y> xs){
        hd = x;
        tail = xs;
    }

    public Y accept(Visitor<X, Y> v) {
        return v.visitCons(hd, tail);

        }

}


abstract class Visitor<X, Y> {

    public Y visit(List<X, Y> l) {
        return l.accept(this);
    }

    abstract Y visitNil();

    abstract Y visitCons(X hd, List<X, Y> tail);

}

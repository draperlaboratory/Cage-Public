
class Nil<X, Y> extends List<X, Y> {

    public Y accept(Visitor<X, Y> v) { return v.visitNil(); }

}

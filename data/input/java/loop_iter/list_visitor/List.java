
abstract class List<X, Y> {

    public abstract Y accept(Visitor<X, Y> v);

}

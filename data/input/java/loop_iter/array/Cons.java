
class Cons<X> extends List<X> {

    X hd;
    List<X> tail;

    public bool find(X x) {
        if (x.equals(hd))
            return true;
        else
            return tail.find(x);

        }

}

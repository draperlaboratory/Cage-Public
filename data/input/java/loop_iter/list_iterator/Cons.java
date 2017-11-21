
class Cons<X> extends List<X> {

    X hd;
    List<X> tail;

    public Cons(X x, List<X> xs){
        hd = x;
        tail = xs;
    }

    public boolean hasNext(){
        return true;
    }

    public X head(){
        return hd;
    }

    public List<X> tail(){
        return tail;
    }

}

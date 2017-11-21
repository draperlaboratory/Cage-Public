
class EmptyList extends Exception {
    public EmptyList() {
        super("Unexpected empty list!");
    }

}

abstract class List<X> implements Iterable {

    public Iterator<X> iterator(){
        return new ListIterator(this);
    }

    public abstract boolean hasNext();

    public X head() throws EmptyList {
        throw new EmptyList();
    }

    public List<X> tail() throws EmptyList {
        throw new EmptyList();
    }

}

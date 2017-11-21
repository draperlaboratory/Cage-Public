interface Iterator<T> {

    public boolean hasNext();

    public T next() throws EmptyList;

}

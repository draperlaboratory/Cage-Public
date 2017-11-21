
class ListIterator<X> implements Iterator<X> {

    private List<X> l;

    //TODO: if l gets updated, does fresh_list
    // get updated as well?
    public ListIterator(List<X> fresh_list){
        l = fresh_list;
    }

    public boolean hasNext(){
        return l.hasNext();
    }

    public X next() throws EmptyList {
        if(l.hasNext()){
            X x = l.head();
            l = l.tail();
            return x;
        }
        else
            throw new EmptyList();
    }
}

public class Bucket{

    private List elements;

    public Bucket(String firstEl){
        this.elements = new List(firstEl);
    }

    public void add(String s){
        this.elements.add(s);
    }

    public boolean contains(String s){
        if(elements == null)
            return false;
        else
            return this.elements.contains(s);
    }

    public void remove(String s){
        this.elements.remove(s);
    }
}

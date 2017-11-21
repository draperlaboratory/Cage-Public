public class MyInt extends Comp{
    private int me = 0;

    public MyInt(int i){
        this.me = i;
    }

    public int comp(MyInt m1){
        if(this.me < m1.me)
            return Comp.SMALLER;
        else if (this.me > m1.me)
            return Comp.LARGER;
        else
            return Comp.EQUAL;
    }

    public int comp(Comp c1){
        if (c1 instanceof MyInt){
            return this.comp((MyInt) c1);
        }else{
            return Comp.INCOMPARABLE;
        }
    }

    public String toString(){
        return "" + this.me;
    }
}

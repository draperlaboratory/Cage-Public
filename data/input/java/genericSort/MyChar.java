public class MyChar extends Comp{
    private char me = (char)0;

    public MyChar(char c){
        this.me = c;
    }

    public MyChar(int i){
        this.me = (char) i;
    }

    public int comp(MyChar c1){
        if(this.me < c1.me){
            return Comp.SMALLER;
        }else if (this.me > c1.me){
            return Comp.LARGER;
        }else{
            return Comp.EQUAL;
        }
    }

    public int comp(Comp c1){
        if (c1 instanceof MyChar){
            return this.comp((MyChar) c1);
        }else{
            return Comp.INCOMPARABLE;
        }
    }

    public String toString(){
        return "" + this.me;
    }
}

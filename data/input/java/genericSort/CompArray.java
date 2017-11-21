public abstract class CompArray extends Sortable{
    protected Comp[] contents;

    public void display(){
        System.out.print("[");
        for(int i = 0; i < this.contents.length; i++){
            System.out.print(" " + this.contents[i]);
        }
        System.out.println(" ]");
    }

    public boolean sorted(int i, int j){
        int ret = this.contents[i].comp(this.contents[j]);
        return ret != Comp.LARGER;
    }

    public Sortable swap(int i, int j){
        Comp temp = this.contents[i];
        this.contents[i] = this.contents[j];
        this.contents[j] = temp;
        return this;
    }

    public int size(){
        return this.contents.length;
    }

    public Comp minElement(){
        Comp min = this.contents[0];
        for(int i = 1; i < this.contents.length; i++){
            Comp el = this.contents[i];
            if (el.comp(min) == Comp.SMALLER){
                min = el;
            }
        }
        return min;
    }

    public int minIndexBeyond(int start){
        int retInd = start;
        for(int i = start; i < this.contents.length; i++){
            if(!this.sorted(retInd,i)){
                retInd = i;
            }
        }
        return retInd;
    }

    public Comp maxElement(){
        Comp max = this.contents[0];
        for(int i = 1; i < this.contents.length; i++){
            Comp el = this.contents[i];
            if (el.comp(max) == Comp.LARGER){
                max = el;
            }
        }
        return max;
    }

    public Comp ith(int i){
        return this.contents[i];
    }
}

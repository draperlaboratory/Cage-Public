public class ArrayList{
    protected Object[] array;
    protected static int factor = 2;

    public ArrayList(int initSize){
        this.array = new Object[initSize];
    }

    public void set(Object o, int i){
        if(this.array.length < i){
            this.resize();
        }
        this.array[i] = o;
    }

    public Object get(int i){
        return this.array[i];
    }

    public void resize(){
        Object[] next = new Object[this.array.length * factor];
        for(int i = 0; i < this.array.length; i++){
            next[i] = this.array[i];
        }
        this.array = next;
    }
}

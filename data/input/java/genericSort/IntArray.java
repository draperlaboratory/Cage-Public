public class IntArray extends CompArray{

    public IntArray(int size){
        this.contents = new MyInt[size];
    }

    public static IntArray revOrder(int size){
        IntArray ret = new IntArray(size);
        for(int i = 0; i < size; i++){
            ret.contents[i] = new MyInt(size - i);
        }
        return ret;
    }
}

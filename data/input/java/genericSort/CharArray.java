public class CharArray extends CompArray{

    public CharArray(int size){
        this.contents = new MyChar[size];
    }

    public static CharArray revOrder(int size){
        CharArray ret = new CharArray(size);
        for(int i = 0; i < size; i++){
            ret.contents[i] = new MyChar(size - i);
        }
        return ret;
    }
}

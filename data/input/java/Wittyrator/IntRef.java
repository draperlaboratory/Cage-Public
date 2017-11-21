public class IntRef{
    private int myInt;
    public IntRef(int i){
	this.myInt = i;
    }
    public void incf(){
	this.myInt ++;
    }
    public void incf(int j){
	this.myInt += j;
    }
    public String toString(){
	return "" + this.myInt;
    }
}

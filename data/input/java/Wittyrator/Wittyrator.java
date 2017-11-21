import java.util.ArrayList;
import java.util.Iterator;


public class Wittyrator{
    private ArrayList<IntRef> elements;
    
    public Wittyrator(String [] args){
	this.elements = new ArrayList<IntRef>();
	for(int i = 0; i < args.length; i++){
	    this.elements.add(new IntRef(args[i].length()));
	}
    }

    public void methodOfInterest(){
	Iterator i = this.elements.iterator();
	while(i.hasNext()){
	    IntRef obj = (IntRef)i.next();
	    obj.incf();
	}
    }

    public void display(){
	Iterator i = this.elements.iterator();
	while(i.hasNext()){
	    System.out.println(i.next().toString());
	}
    }
    
    public static void main(String [] argv){
	Wittyrator w = new Wittyrator(argv);
	w.display();
	w.methodOfInterest();
	w.display();
    }
}

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

public class MyList{
	
    public static void main(String [] argv){
	List<String> al = Arrays.asList(argv);
	Iterator it = al.iterator();
	while(it.hasNext()){
	    System.out.println(it.next());
	}
    }
}

import java.util.Iterator;
import java.util.LinkedList;

public class IteratorExample {
    public void iterate(Iterator it) {
        while (it.hasNext()) it.next();
    }

    public void multipleIterate(Iterator it) {
        while (it.hasNext()) it.next();
        it = getIterator();
        while (it.hasNext()) it.next();
    }

    public Iterator getIterator() {
        return new LinkedList().iterator();
    }

    public void nestedIterate(Iterator it) {
        while (it.hasNext()) {
            Iterator it2 = getIterator();
            while (it2.hasNext()) it2.next();
            it.next();
        }
    }
}

import java.util.Iterator;
import java.util.Arrays;

class ConstOrLinear {

    

    public void run(boolean b, Iterator it) {
        if (b) {
            while (it.hasNext()) {
                it.next();
            }
        } else { }
    }


    public static void main(String [] args) {
        ConstOrLinear c = new ConstOrLinear();
        c.run(false, Arrays.asList(args).iterator());
    }
}

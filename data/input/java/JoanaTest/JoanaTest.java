import java.util.concurrent.FutureTask;

public class JoanaTest {

    static A a = new A();
    
    public static void main(String[] args) {
        FutureTask t = new FutureTask(a, null);
        try {
            t.run();
        }
        catch (Throwable e) {}
    }
}


class A implements Runnable {
    
    public void run(){
        System.out.println("Hello, world!");
    }
    
}

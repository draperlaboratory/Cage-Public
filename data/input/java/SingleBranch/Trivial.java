public class Trivial{

    public static void infinite(){
        int i = 0;
        while(true){
            i ++;
        }
    }

    public static void constant(){
        int i = 10;
        while(i > 0){
            i --;
        }
    }

    public static void dispatch(int len){
        if (len > 5){
            Trivial.infinite();
        }else{
            Trivial.constant();
        }
    }

    public static void main(String[] argv){
        int len = argv.length;
        Trivial.dispatch(len);
    }
}

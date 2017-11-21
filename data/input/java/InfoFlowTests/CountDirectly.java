public class CountDirectly{
    public static void count(int c){
        for(int i = 0; i < c; i++){}
    }

    public static void count1(int c){
        CountDirectly.count(c);
    }

    public static void count0(int c){
        CountDirectly.count1(c);
    }


    public static void main(String[] argv){
        int i = argv.length;
        CountDirectly.count0(i);
    }
}

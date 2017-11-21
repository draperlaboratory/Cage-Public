public class TestString {
    //constant
    public int comp(String s1){
        String s2 = "Equal";
        if(s1 == s2){
            return 0;
        }else{
            return 1;
        }
    }

    public static void main(String [] args){
        TestString t = new TestString();
        if(args.length == 0){
            System.out.println("O(1)");
            return;
        }
        if (0 == t.comp(args[0])){
            System.out.println("Equal");
        }else{
            System.out.println("Not Equal");
        }
    }
}

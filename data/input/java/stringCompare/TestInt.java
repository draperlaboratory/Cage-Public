public class TestInt {
    //constant
    public int comp(int i1){
        int i2 = 42;
        if(i1 == i2){
            return 0;
        }else{
            return 1;
        }
    }

    public void main(String [] args){
        TestInt t = new TestInt();
        if(args.length == 0){
            System.out.println("O(1)");
            return;
        }
        if (0 == t.comp(args[0].length())){
            System.out.println("42 Chars");
        }else{
            System.out.println("Not 42 Chars");
        }
    }
}

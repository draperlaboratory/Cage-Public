public class OddExceptions{

    // constant
    private void someInternalProcess(int i) throws OddException{
        if(i % 2 == 0){
            //            System.out.println("Even");
        }else{
            throw new OddException();
        }
    }

    // non-terminating if increment is 0
    // O(n) otherwise
    public void loop(int startIndex, int increment, int endIndex){
        for(int i = startIndex; i < endIndex; i+= increment){
            try{
                this.someInternalProcess(i);
            }catch(OddException oe){
                //                System.out.println("odd");
            }
        }
    }

    //non-terminating
    public static void main(String[] argv){
        if(argv.length == 0){
            OddExceptions me = new OddExceptions();
            me.loop(argv.length, 0, 512);
        }else{
            OddExceptions me = new OddExceptions();
            me.loop(argv.length, argv[0].length(), 512);
        }
    }
}

public class MultipleInsertsLinear {

    //O(n)
    public static void main(String[] argv){
        if(argv.length == 0){
            System.out.println("O(n)");
        }else{
            LinkedList myList;
            LinkedList insertPointer;
            int i;
            myList = new LinkedList(argv.length);
            insertPointer = myList;
            for(i = 0; i < argv.length; i++){
                insertPointer = insertPointer.append(argv[i].length());
            }
        }
    }
}

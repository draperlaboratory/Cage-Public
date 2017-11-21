public class MultipleInserts {

    //O(n^2)
    public static void main(String[] argv){
        if(argv.length == 0){
            System.out.println("O(n^2)");
        }else{
            LinkedList myList;
            int i;
            myList = new LinkedList(argv.length);
            for(i = 0; i < argv.length; i++){
                myList.append(argv[i].length());
            }
        }
    }
}

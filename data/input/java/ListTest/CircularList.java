public class CircularList{
    // non-terminating
    public static void main(String[] argv){
        LinkedList myList = new LinkedList(); // makes a circular list.
        myList.setMe(argv.length);
        if(argv.length == 0){
            System.out.println("non-terminating");
        }else if(argv.length > 0){
            myList.find(argv[0].length());
        }
    }
}

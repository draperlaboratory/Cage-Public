public class ArrayListLeak{
    private static String[] data =
        new String[]{"Alice", "Bob", "Carol", "Duke", "Edward", "Francine",
                     "Gerald", "Harriette", "Inigo Montoya", "Jordan",
                     "Kristine", "Lucy", "Mary", "Nancy", "Oron", "Patty",
                     "Quixote", "Roberta", "Samantha", "Theodore", "Uwe",
                     "Vonnegut", "Wilson", "Xander", "Zeddidiah",
                     "Amber", "Alex", "Ashley", "Anthony", "Antwon", "Archie",
                     "Aaron", "Axle Rose", "Anne", "Amy", "Arturo", "Avon"};
    // the secret is how much we padded the array list by.
    private static int secret = 10;

    public static void main(String[] argv){
        ArrayList al = new ArrayList(data.length + secret);
        int i = 0;
        for(; i < data.length; i++){
            al.set(data[i],i);
        }
        for(int j = 0; j < argv.length; j++){
            al.set(argv[j],i+j);
        }
    }
}

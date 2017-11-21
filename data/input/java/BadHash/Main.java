public class Main{
    private static String[] secret =
        new String[]{"Alice", "Bob", "Carol", "Duke", "Edward", "Francine",
                     "Gerald", "Harriette", "Inigo Montoya", "Jordan",
                     "Kristine", "Lucy", "Mary", "Nancy", "Oron", "Patty",
                     "Quixote", "Roberta", "Samantha", "Theodore", "Uwe",
                     "Vonnegut", "Wilson", "Xander", "Zeddidiah",
                     // the secreet is there are more A names than all others!
                     "Amber", "Alex", "Ashley", "Anthony", "Antwon", "Archie",
                     "Aaron", "Axle Rose", "Anne", "Amy", "Arturo", "Avon"};

    public static void main(String[] argv){

        HashTable h = new HashTable(17);
        for(int i = 0; i < secret.length; i++){
            h.add(secret[i]);
        }

        for(int i = 0; i < argv.length; i++){
            h.add(argv[i]);
        }
    }
}

import java.util.Scanner;

public class Serve {

    public static boolean serve(ServerData server) {
        @SuppressWarnings("resource")
            Scanner reader = new Scanner(System.in);
        System.out.println("Enter a command: [add, view, quit]");
        String sentence = reader.nextLine();
        if (sentence.equals("add")) {
            System.out.println("Enter a sequence of words: ");
            sentence = reader.nextLine();
            String[] words = sentence.split(" ");
            for (String word : words) {
                boolean responseValue = server.addPublic(word);
                String responseString = Response.respond(responseValue, word);
                System.out.println(responseString);
            }
        }
	
        else if (sentence.equals("view")) {
            System.out.println("Server contents:\n");
            System.out.println(server.viewPublic());
        }
	
        else if (sentence.equals("quit")) {
            System.out.println("Quitting...");
            return false;
        }
	
        else {
            System.out.println("Unkown command");
        }
        return true;
	
    }
    
    
    
}

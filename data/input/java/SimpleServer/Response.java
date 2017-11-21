
public class Response {

	public static String respond(boolean responseValue, String word) {
		if (responseValue) {
			return "Succeeded in adding " + word + " to the server.";
		}
		else {
			return "Failed to add " + word + " to the server.";
		}
	}
	
	
	
}

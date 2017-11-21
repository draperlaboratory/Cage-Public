
public class SimpleServer {

	public static void main(String[] args) {
		
		ServerData server = new ServerData();
		
		boolean serverIsRunning = true;
		
		while (serverIsRunning) {
			serverIsRunning = Serve.serve(server);
		}
				
	}

}

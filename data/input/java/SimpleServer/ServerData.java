public class ServerData {

	private HashSet publicData;
			
	public ServerData() {
            publicData = new HashSet();
	}
	
	public boolean addPublic(String s) {
            return publicData.add(s);
	}
	
	public String viewPublic() {
            return this.publicData.toString();
	}
	
}

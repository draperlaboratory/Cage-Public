
public class SingletonUnit {
	private static SingletonUnit instance = null;
	protected SingletonUnit() {
	}
	public static SingletonUnit getInstance() {
		if (instance == null) {
			instance = new SingletonUnit();
		}
		return instance;
	}
}
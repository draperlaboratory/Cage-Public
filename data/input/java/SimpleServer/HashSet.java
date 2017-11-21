public class HashSet {

    private static final int SIZE = 10;
    private StringList[] data;

    public HashSet() {
        this.data = new StringList[SIZE];
    }

    public boolean add(String s) {
        int sHash = Math.abs(s.hashCode() % SIZE);
        if(data[sHash] == null) {
            data[sHash] = StringList.singleton(s);
        } else {
            data[sHash].append(s);
        }
        return true;
    }

    public String toString() {
        String s = "";
        for(int i = 0; i < SIZE; i++) {
            if(data[i] != null) {
                s += i;
                s += ": [" + data[i].toString() + " ]";
            }
        }
        return s;
    }
        
        
    

}
